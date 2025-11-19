# rich_structural_representation.py
# Analyse structurelle "rich" d'un fichier Scala :
# - Représentation uniquement basée sur les structures de contrôle
# - Signatures structurelles de chaque fonction
# - Structure hiérarchique annotée (SINGLE / MULTI / EMPTY)
# - Chemins de contrôle (control-flow paths)
#
# PRÉREQUIS :
# - tree-sitter installé
# - librairie Scala compilée dans build/my-languages.so
#
# Exemple build :
#   Language.build_library(
#       "build/my-languages.so",
#       ["path/to/tree-sitter-scala"]
#   )

from tree_sitter import Language, Parser
from tree_sitter_languages import get_language


###############################################################################
# 1. Initialisation Tree-sitter
###############################################################################

SCALA_LANGUAGE = get_language("scala")
parser = Parser()
parser.set_language(SCALA_LANGUAGE)


def parse_code(code: str):
    return parser.parse(bytes(code, "utf8")).root_node


###############################################################################
# 2. Extraction des fonctions
###############################################################################

def extract_name(node):
    """Extrait le nom (identifier) d'une définition de fonction."""
    for child in node.children:
        if child.type == "identifier":
            return child.text.decode("utf8")
    return None


def extract_function_body(node):
    """Retourne le sous-arbre correspondant au corps de la fonction."""
    # L'AST Scala varie selon la grammaire exacte, donc on reste large
    for child in node.children:
        if child.type in {"block", "expression"}:
            return child
    return node


def collect_functions(root):
    """Renvoie un dict : nom_fonction -> node AST."""
    functions = {}
    stack = [root]

    while stack:
        node = stack.pop()
        if node.type == "function_definition":
            name = extract_name(node)
            if name:
                functions[name] = node
        stack.extend(node.children)

    return functions


###############################################################################
# 3. Réduction structurelle (contrôles + appels connus)
###############################################################################

# Nœuds de structure de contrôle qu'on veut garder
CONTROL_NODES = {
    "if_expression",
    "else_clause",
    "else_if_clause",
    "for_expression",
    "while_expression",
    "match_expression",
    "case_clause",
    "try_expression",
    "catch_clause",
    "finally_clause",
    "return_statement",
    "function_definition",
}

# Nœuds représentant un appel
CALL_NODE_TYPES = {
    "call_expression",
    "function_call",
}


def extract_call_name(node):
    """Extrait le nom de la fonction appelée (si identifiable simplement)."""
    for child in node.children:
        if child.type == "identifier":
            return child.text.decode("utf8")
    return None


def clean_children(children):
    """Supprime les seq vides et les None."""
    cleaned = []
    for c in children:
        if c is None:
            continue
        if c["type"] == "SEQ" and not c["children"]:
            continue
        cleaned.append(c)
    return cleaned


def reduce_struct(node, functions, depth=0, max_depth=4):
    """
    Réduit l'AST pour ne garder que :
      - les structures de contrôle
      - les appels à des fonctions définies par l'étudiant (CALL_nom)
    Les appels vers des fonctions inconnues (standard Scala) sont ignorés.
    """

    if depth > max_depth:
        # En cas de profondeur excessive (récursion, etc.)
        return {"type": "DEPTH_LIMIT", "children": []}

    # 1) Appels de fonctions
    if node.type in CALL_NODE_TYPES:
        call_name = extract_call_name(node)
        if call_name in functions:
            # On garde la trace de l'appel mais SANS inline complet (ici version "rich" non-inline)
            return {"type": f"CALL_{call_name}", "children": []}
        # Appel inconnu -> fonction standard / librairie -> ignorée
        return None

    # 2) Structures de contrôle
    if node.type in CONTROL_NODES:
        children = clean_children([
            reduce_struct(child, functions, depth, max_depth)
            for child in node.children
        ])
        return {"type": node.type, "children": children}

    # 3) Nœuds "intermédiaires" -> propagation
    children = clean_children([
        reduce_struct(child, functions, depth, max_depth)
        for child in node.children
    ])

    if children:
        return {"type": "SEQ", "children": children}

    return None


###############################################################################
# 4. Analyse structurelle rich
###############################################################################

def is_control_type(node_type: str) -> bool:
    """Retourne True si c'est un nœud de contrôle (hors SEQ / CALL)."""
    return node_type in CONTROL_NODES


def block_type(node: dict) -> str:
    """
    Détermine le "type" du bloc :
      - EMPTY : aucun enfant de contrôle
      - SINGLE : un seul enfant de contrôle
      - MULTI : plusieurs
    """
    if not node or "children" not in node:
        return "EMPTY"

    # On compte les enfants qui sont des contrôles ou des appels
    count = 0
    for child in node["children"]:
        t = child["type"]
        if t.startswith("CALL_") or is_control_type(t):
            count += 1

    if count == 0:
        return "EMPTY"
    elif count == 1:
        return "SINGLE"
    else:
        return "MULTI"


def flatten_control_types(node):
    """
    Retourne une liste (pré-ordre) des types de contrôle rencontrés
    (sans SEQ, mais avec CALL_).
    """
    if not node:
        return []

    types = []
    t = node["type"]

    if t != "SEQ":
        types.append(t.upper())

    for child in node.get("children", []):
        types.extend(flatten_control_types(child))

    return types


def pretty_print_struct(node, indent=0, lines=None):
    """
    Produit une représentation textuelle hiérarchique avec type de bloc.
    """
    if lines is None:
        lines = []

    if not node:
        return lines

    t = node["type"]

    if t == "SEQ":
        # On ne l'affiche pas explicitement, on traverse seulement
        for child in node.get("children", []):
            pretty_print_struct(child, indent, lines)
        return lines

    # Contrôle ou appel
    if t.startswith("CALL_"):
        line = "  " * indent + t.upper()
    else:
        bt = block_type(node)
        line = "  " * indent + f"{t.upper()}[{bt}]"

    lines.append(line)

    for child in node.get("children", []):
        pretty_print_struct(child, indent + 1, lines)

    return lines


def collect_control_paths(node, current=None, paths=None):
    """
    Collecte tous les "chemins de contrôle" du nœud racine jusqu'aux feuilles
    en ignorant les nœuds SEQ.
    """
    if current is None:
        current = []
    if paths is None:
        paths = []

    if not node:
        return paths

    t = node["type"]

    # On ignore SEQ comme vrai nœud de chemin
    new_path = list(current)
    if t != "SEQ":
        new_path = new_path + [t.upper()]

    children = [c for c in node.get("children", []) if c is not None]

    # feuille structurelle : pas d'enfant "non-SEQ" ?
    if not children:
        if new_path:
            paths.append(new_path)
        return paths

    # sinon, on descend
    for child in children:
        collect_control_paths(child, new_path, paths)

    return paths


###############################################################################
# 5. Construction d'une représentation "rich" globale
###############################################################################

def function_structural_signature(func_name, func_node, functions):
    """
    Renvoie une signature structurelle compacte d'une fonction :
    ex : "IF FOR RETURN CALL_FOO"
    """
    body = extract_function_body(func_node)
    reduced = reduce_struct(body, functions)
    types = flatten_control_types(reduced)
    if not types:
        return "EMPTY"
    return " ".join(types)


def build_rich_structural_representation(code: str) -> str:
    """
    Pipeline complet :
      - parse code
      - collecte fonctions
      - réduit l'AST globale
      - construit :
        * signatures de fonctions
        * structure hiérarchique globale
        * chemins de contrôle
    Renvoie une string structurée prête à être utilisée comme input d'embedding.
    """
    root = parse_code(code)
    functions = collect_functions(root)
    reduced_root = reduce_struct(root, functions)

    lines = []

    # 1) Signatures de fonctions
    lines.append("## FUNCTIONS")
    if not functions:
        lines.append("NO_FUNCTIONS")
    else:
        for fname, fnode in functions.items():
            sig = function_structural_signature(fname, fnode, functions)
            lines.append(f"FUNC {fname}: {sig}")

    lines.append("")  # ligne vide

    # 2) Structure hiérarchique globale
    lines.append("## MAIN_STRUCTURE")
    if reduced_root:
        struct_lines = pretty_print_struct(reduced_root, indent=0)
        if struct_lines:
            lines.extend(struct_lines)
        else:
            lines.append("EMPTY_STRUCTURE")
    else:
        lines.append("EMPTY_STRUCTURE")

    lines.append("")

    # 3) Chemins de contrôle
    lines.append("## CONTROL_PATHS")
    if reduced_root:
        paths = collect_control_paths(reduced_root)
        if paths:
            for p in paths:
                lines.append("PATH: " + " -> ".join(p))
        else:
            lines.append("NO_PATHS")
    else:
        lines.append("NO_PATHS")

    lines.append("")

    # 4) Résumé par fonction (identique aux signatures mais groupé)
    lines.append("## FUNCTION_SUMMARIES")
    if not functions:
        lines.append("NO_FUNCTIONS")
    else:
        for fname, fnode in functions.items():
            sig = function_structural_signature(fname, fnode, functions)
            lines.append(f"{fname}: {sig}")

    # On assemble le tout
    return "\n".join(lines)


###############################################################################
# 6. Exemple d'utilisation
###############################################################################

if __name__ == "__main__":
    example_code = """
object Main {

  import scala.io.StdIn._
  import scala.collection.mutable.ArrayBuffer
  import scala.io.Source
  import java.io.{File, PrintWriter}
  import scala.util.{Try, Success, Failure}

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      ingredient.toLowerCase match {
        case "milk" => milk += amount
        case "sugar" => sugar += amount
        case "coffee" => coffee += amount
        case _ => println("Ingrédient invalide.")
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      ingredient.toLowerCase match {
        case "milk" if milk >= amount =>
          milk -= amount
          true
        case "sugar" if sugar >= amount =>
          sugar -= amount
          true
        case "coffee" if coffee >= amount =>
          coffee -= amount
          true
        case _ =>
          println("Quantité insuffisante ou ingrédient invalide.")
          false
      }
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    Try {
      for (line <- Source.fromFile(filename).getLines().drop(1)) {
        val cols = line.split(",").map(_.trim)
        val machine = new Machine(
          id = machines.length + 1, // Assignation automatique des ID
          pincode = cols(0),
          milk = cols(1).toInt,
          sugar = cols(2).toInt,
          coffee = cols(3).toInt
        )
        machines += machine
      }
    } match {
      case Success(_) => println(s"${machines.length} machine(s) chargée(s) avec succès.")
      case Failure(e) => println(s"Erreur lors du chargement du fichier CSV: ${e.getMessage}")
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    Try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
    } match {
      case Success(_) => println("Fichier sauvegardé avec succès.")
      case Failure(e) => println(s"Erreur lors de la sauvegarde du fichier CSV: ${e.getMessage}")
    }
  }

  def main(args: Array[String]): Unit = {
    // Charger les machines depuis le fichier CSV
    val machines = loadcsv("machines.csv")

    // Menu principal
    var mode: Int = 0
    while (mode != 3) {
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")

      mode = readInt()

      if (mode == 1) {
        // Mode Client
        println("Veuillez sélectionner une machine :")
        machines.zipWithIndex.foreach { case (machine, index) =>
          println(s"${index + 1}) Machine ${machine.id}")
        }
        print("> ")

        val machineIndex = readInt() - 1

        if (machineIndex < 0 || machineIndex >= machines.length) {
          println("Identifiant de machine invalide.")
        } else {
          serveClient(machineIndex, machines)
        }

      } else if (mode == 2) {
        // Mode Admin
        println("Mode Admin")
        println("Veuillez sélectionner une machine :")
        machines.zipWithIndex.foreach { case (machine, index) =>
          println(s"${index + 1}) Machine ${machine.id}")
        }
        print("> ")

        val machineIndex = readInt() - 1

        if (machineIndex < 0 || machineIndex >= machines.length) {
          println("Identifiant de machine invalide.")
        } else {
          if (validatePin(machineIndex, machines)) {
            println("Accès autorisé.")
            println("1) Réapprovisionner les stocks")
            println("2) Mettre à jour le code PIN")
            print("> ")

            val choixAdmin = readInt()

            if (choixAdmin == 1) {
              restockMachine(machineIndex, machines)
            } else if (choixAdmin == 2) {
              updatePin(machineIndex, machines)
            } else {
              println("Choix invalide.")
            }
          } else {
            println("Code PIN incorrect. Accès refusé.")
          }
        }
      }
    }

    // Sauvegarder les machines dans le fichier CSV
    savecsv("machines.csv", machines)

    println("Merci d'avoir utilisé le distributeur Nospresso Café. À bientôt !")
  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    val machine = machines(machineId)

    println("Veuillez sélectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    val choixBoisson = readInt()

    var prixFinal = BigDecimal(0)
    var boisson = ""
    var cafeNecessaire = 0
    var laitNecessaire = 0

    choixBoisson match {
      case 1 =>
        boisson = "Expresso"
        prixFinal = 2.0
        cafeNecessaire = 8
      case 2 =>
        boisson = "Cappuccino"
        prixFinal = 2.5
        cafeNecessaire = 6
        laitNecessaire = 100
      case 3 =>
        println("Veuillez sélectionner la taille de votre Latte :")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")
        print("> ")

        readInt() match {
          case 1 =>
            boisson = "Latte (Petit)"
            prixFinal = 2.7
            cafeNecessaire = 6
            laitNecessaire = 120
          case 2 =>
            boisson = "Latte (Moyen)"
            prixFinal = 3.2
            cafeNecessaire = 8
            laitNecessaire = 150
          case 3 =>
            boisson = "Latte (Grand)"
            prixFinal = 3.7
            cafeNecessaire = 12
            laitNecessaire = 200
          case _ =>
            println("Entrée invalide.")
            return
        }
      case _ =>
        println("Entrée invalide.")
        return
    }

    println(s"Boisson sélectionnée : $boisson")
    println(s"Prix de la boisson : $prixFinal CHF")

    // Gestion du sucre
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    val sucreNecessaire = readInt() match {
      case 1 => 0
      case 2 =>
        prixFinal += 0.10
        5
      case 3 =>
        prixFinal += 0.20
        10
      case 4 =>
        prixFinal += 0.30
        15
      case _ =>
        println("Entrée invalide.")
        return
    }

    // Vérification des stocks
    if (machine.coffee >= cafeNecessaire && machine.sugar >= sucreNecessaire && machine.milk >= laitNecessaire) {
      // Mise à jour des stocks
      machine.removeIngredient("coffee", cafeNecessaire)
      machine.removeIngredient("sugar", sucreNecessaire)
      machine.removeIngredient("milk", laitNecessaire)

      // Simulation du paiement et de la préparation
      println(s"Prix total : $prixFinal CHF")
      println("Veuillez payer en utilisant Twint.")
      Thread.sleep(3000)
      println("Paiement confirmé.")
      println("Préparation de votre boisson...")
      Thread.sleep(5000)
      println("Votre boisson est prête ! Bonne dégustation !")

    } else {
      println("Erreur : Quantité insuffisante pour préparer la boisson sélectionnée.")
      println("Veuillez vérifier les stocks ou choisir une autre boisson.")
    }
  }

  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    val machine = machines(machineId)
    var attempts = 0
    while (attempts < 3) {
      print("Entrez le code PIN : ")

      if (readLine() == machine.pincode) return true
      println("Code PIN incorrect.")
      attempts += 1
    }
    false
  }

  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    val machine = machines(machineId)
    print("Entrez le nouveau code PIN (6 chiffres) : ")
    val newPin = readLine()
    if (newPin.length == 6 && newPin.forall(_.isDigit)) {
      machine.pincode = newPin
      println("Code PIN mis à jour avec succès.")
    } else {
      println("Entrée invalide. Le code PIN doit comporter exactement 6 chiffres.")
    }
  }

  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    val machine = machines(machineId)
    println("Réapprovisionnement des stocks...")
    println("Quel élément souhaitez-vous réapprovisionner ?")
    println("1) Poudre de café")
    println("2) Sucre")
    println("3) Lait")
    print("> ")

    readInt() match {
      case 1 =>
        println("Veuillez entrer la quantité de poudre de café à ajouter (en grammes) : ")
        machine.addIngredient("coffee", readInt())
        println(s"Stocks mis à jour : ${machine.coffee} g de café")
      case 2 =>
        println("Veuillez entrer la quantité de sucre à ajouter (en grammes) : ")
        machine.addIngredient("sugar", readInt())
        println(s"Stocks mis à jour : ${machine.sugar} g de sucre")
      case 3 =>
        println("Veuillez entrer la quantité de lait à ajouter (en millilitres) : ")
        machine.addIngredient("milk", readInt())
        println(s"Stocks mis à jour : ${machine.milk} ml de lait")
      case _ => println("Choix invalide.")
    }
  }
}
    """

    rep = build_rich_structural_representation(example_code)
    print(rep)
