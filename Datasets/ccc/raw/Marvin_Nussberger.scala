import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{FileWriter, PrintWriter}
import scala.util.Random
import scala.io.StdIn._

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = ingredient.toLowerCase match {
    case "milk"   => milk += amount
    case "sugar"  => sugar += amount
    case "coffee" => coffee += amount
    case _        => println("Ingrédient non reconnu.")
  }
  def removeIngredient(ingredient: String, amount: Int): Boolean = ingredient.toLowerCase match {
    case "milk"   if milk   >= amount => milk   -= amount; true
    case "sugar"  if sugar  >= amount => sugar  -= amount; true
    case "coffee" if coffee >= amount => coffee -= amount; true
    case _ =>
      println(s"Stock insuffisant pour $ingredient.")
      false
  }
  override def toString: String =
    "ID: " + id +
      ", PIN=" + pincode +
      ", Lait=" + (milk / 1000.0) + "L" +
      ", Sucre=" + sugar + "g" +
      ", Café=" + coffee + "g"
}
object Nospresso {
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    println("Chargement des machines depuis " + filename + "...")
    try {
      val source = Source.fromFile(filename)
      val lines  = source.getLines().drop(1)
      for ((line, index) <- lines.zipWithIndex) {
        val Array(pincode, milk, sugar, coffee) = line.split(",")
        val machine = new Machine(index + 1, pincode, milk.toInt, sugar.toInt, coffee.toInt)
        machines += machine
        println("Machine " + (index + 1) + " chargée :")
        println("ID: " + (index + 1))
        println("Code PIN: " + pincode)
        println("Lait: " + (milk.toInt / 1000.0) + "L")
        println("Sucre: " + sugar.toInt + "g")
        println("Café: " + coffee.toInt + "g\n")
      }
      println(machines.size + " machine(s) chargée(s) avec succès.")
      source.close()
    } catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Fichier '" + filename + "' non trouvé.")
      case _: Exception =>
        println("Erreur : Impossible de lire le fichier '" + filename + "'.")
    }
    machines
  }
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val fw = new PrintWriter(new FileWriter(filename))
      fw.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        fw.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
      }
      fw.close()
      println("Les machines ont été sauvegardées correctement dans le fichier " + filename + ".")
    } catch {
      case _: Exception =>
        println("Erreur : Impossible de sauvegarder les données dans le fichier '" + filename + "'.")
    }
  }
  def validePin(machine: Machine): Boolean = {
    var essai = 3
    while (essai > 0) {
      println("Entrez le code PIN : ")
      val pinSaisi = readLine()
      if (pinSaisi == machine.pincode) {
        println("Accès accordé à la Machine " + machine.id + ".")
        return true
      } else {
        essai -= 1
        if (essai > 0) {
          println("Code incorrect. Tentatives restantes : " + essai)
        } else {
          println("Code PIN incorrect. 0 tentatives restantes.")
          return false
        }
      }
    }
    false
  }
  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    val machines = loadcsv(filename)
    if (machines.isEmpty) {
      println("Aucune machine chargée. Vérifiez le fichier CSV.")
      return
    }
    var encore = true
    while (encore) {
      println("\nNospresso Café")
      println("1) Mode Client")
      println("2) Mode Admin")
      println("3) Quitter")
      print("> ")
      val choix = readLine().toInt
      choix match {
        case 1 => modeClient(machines)
        case 2 => modeAdmin(machines)
        case 3 =>
          savecsv(filename, machines)
          encore = false
        case _ => println("Option invalide.")
      }
    }
  }
  def modeClient(machines: ArrayBuffer[Machine]): Unit = {
    println("Sélectionnez la machine (1-" + machines.size + ") >")
    val idMachine = readInt() - 1
    if (idMachine >= 0 && idMachine < machines.size) {
      val machine = machines(idMachine)
      println("Accès à la Machine " + machine.id + " accordé.")
      println("Choisissez votre boisson :")
      println("1) Expresso (2.00 CHF)")
      println("2) Cappuccino (2.50 CHF)")
      println("3) Latte (à partir de 3.00 CHF)")
      val boisson = readInt()
      boisson match {
        case 1 => prepareDrink(machine, "Expresso", 8,  0,   2.0, laitsupp = false)
        case 2 => prepareDrink(machine, "Cappuccino", 6, 100, 2.5, laitsupp = true)
        case 3 =>
          println("Choisissez la taille : (1) Petit (3.00 CHF), (2) Moyen (3.50 CHF), (3) Grand (4.00 CHF)")
          val taille = readInt()
          taille match {
            case 1 => prepareDrink(machine, "Latte (Petit)",  6,  120, 3.0, laitsupp = true)
            case 2 => prepareDrink(machine, "Latte (Moyen)",  8,  150, 3.5, laitsupp = true)
            case 3 => prepareDrink(machine, "Latte (Grand)",  12, 200, 4.0, laitsupp = true)
            case _ => println("Taille invalide.")
          }
        case _ => println("Choix de boisson invalide.")
      }
    } else {
      println("Numéro de machine invalide.")
    }
  }
  def modeAdmin(machines: ArrayBuffer[Machine]): Unit = {
    println("Sélectionnez la machine (1-" + machines.size + ") >")
    val idMachine = readInt() - 1
    if (idMachine >= 0 && idMachine < machines.size) {
      val machine = machines(idMachine)
      if (validePin(machine)) {
        println(machine.toString)
        println("1) Réapprovisionnement des stocks")
        println("2) Changer le code PIN")
        val choix = readInt()
        choix match {
          case 1 => ajoutstock(machine)
          case 2 => Pinchange(machine)
          case _ => println("Option invalide.")
        }
      }
    } else {
      println("Numéro de machine invalide.")
    }
  }
  def prepareDrink(machine: Machine,
                   nomboi: String,
                   cafeness: Int,
                   laitness: Int,
                   prix: Double,
                   laitsupp: Boolean): Unit = {
    if (!machine.removeIngredient("coffee", cafeness)) return
    if (!machine.removeIngredient("milk",   laitness))   return
    var laitplus     = 0
    var laitplusp = 0.0
    if (laitsupp) {
      println("Souhaitez-vous ajouter du lait supplémentaire ? (1) Oui / (2) Non :")
      val ans = readInt()
      if (ans == 1) {
        println("Combien de doses supplémentaires ? (1 à 3 doses, 50 ml chacune)")
        val doses = readInt()
        if (doses >= 1 && doses <= 3) {
          laitplus     = 50 * doses
          laitplusp = 0.05 * doses
        } else {
          println("Nombre de doses invalide, pas de lait supplémentaire.")
        }
      }
    }
    println("Souhaitez-vous ajouter du sucre ? (1) Sans, (2) Peu(5g), (3) Moyen(10g), (4) Beaucoup(15g)")
    val sucre = readInt()
    val Sucreplus = Array(0, 0, 5, 10, 15)
    val sucrep   = Array(0.0, 0.0, 0.1, 0.2, 0.3)

    val Sucreq = if (sucre >= 1 && sucre <= 4) Sucreplus(sucre) else 0
    val sucreprix   = if (sucre >= 1 && sucre <= 4) sucrep(sucre)   else 0.0

    if (!machine.removeIngredient("milk", laitplus)) return
    if (!machine.removeIngredient("sugar", Sucreq)) return
    val prixfin = prix + laitplusp + sucreprix
    twint(prixfin, nomboi)
  }
  def twint(cout: Double, nomboi: String): Unit = {
    val code = Random.alphanumeric.take(5).mkString.toUpperCase
    println(f"Veuillez payer en utilisant Twint. Prix total : $cout%.2f CHF")
    println("Votre code de paiement est : " + code)
    println("En attente de validation du paiement...")
    Thread.sleep(3000)
    println("Paiement accepté !")
    println("Votre boisson " + nomboi + " est prête. Bonne dégustation !")
  }
  def ajoutstock(machine: Machine): Unit = {
    println("Quantité de lait à ajouter (ml) :")
    machine.addIngredient("milk", readInt())
    println("Quantité de sucre à ajouter (g) :")
    machine.addIngredient("sugar", readInt())
    println("Quantité de café à ajouter (g) :")
    machine.addIngredient("coffee", readInt())
    println("Stocks mis à jour avec succès.")
  }
  def Pinchange(machine: Machine): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres :")
    val pin = readLine()
    if (pin.length == 6 && pin.forall(_.isDigit)) {
      machine.pincode = pin
      println("Code PIN mis à jour avec succès.")
    } else {
      println("Code PIN invalide. Doit comporter 6 chiffres.")
    }
  }
}