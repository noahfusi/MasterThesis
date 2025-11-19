import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer

object Main {

  class Machine(val id: Int, var codePIN: String, var lait: Int, var sucre: Int, var cafe: Int) {

    def ajouterIngredient(ingredient: String, quantite: Int): Unit = {
      if (quantite > 0) {
        ingredient match {
          case "lait" => lait += quantite; println(s"Lait ajouté : $quantite ml. Nouveau stock : $lait ml.")
          case "sucre" => sucre += quantite; println(s"Sucre ajouté : $quantite g. Nouveau stock : $sucre g.")
          case "cafe" => cafe += quantite; println(s"Café ajouté : $quantite g. Nouveau stock : $cafe g.")
          case _ => println("Ingrédient inconnu.")
        }
      } else {
        println("Quantité invalide.")
      }
    }

    def retirerIngredient(ingredient: String, quantite: Int): Boolean = {
      if (quantite > 0) {
        ingredient match {
          case "lait" if lait >= quantite =>
            lait -= quantite
            println(s"Lait retiré : $quantite ml. Nouveau stock : $lait ml.")
            true
          case "sucre" if sucre >= quantite =>
            sucre -= quantite
            println(s"Sucre retiré : $quantite g. Nouveau stock : $sucre g.")
            true
          case "cafe" if cafe >= quantite =>
            cafe -= quantite
            println(s"Café retiré : $quantite g. Nouveau stock : $cafe g.")
            true
          case "lait" =>
            println(s"Stock insuffisant de lait : disponible $lait ml, requis $quantite ml.")
            false
          case "sucre" =>
            println(s"Stock insuffisant de sucre : disponible $sucre g, requis $quantite g.")
            false
          case "cafe" =>
            println(s"Stock insuffisant de café : disponible $cafe g, requis $quantite g.")
            false
          case _ =>
            println("Ingrédient inconnu.")
            false
        }
      } else {
        println("Quantité invalide.")
        false
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val machines = chargerCSV("machines.csv")
    println(s"${machines.size} machine(s) chargée(s) avec succès.")

    var programmeEnCours = true
    while (programmeEnCours) {
      println("Veuillez sélectionner une machine :")
      var idMachine = readInt() - 1
      while (idMachine < 0 || idMachine >= machines.size) {
        println(s"Entrée invalide. Veuillez choisir une machine entre 1 et ${machines.size} :")
        idMachine = readInt() - 1
      }

      val machine = machines(idMachine)
      println("\n        Nospresso Café        ")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      val mode = readInt()

      mode match {
        case 1 => servirClient(machine)
        case 2 => modeAdmin(machine)
        case 3 =>
          println("Sauvegarde des machines en cours...")
          sauvegarderCSV("machines.csv", machines)
          println("Programme terminé.")
          programmeEnCours = false
        case _ => println("Entrée invalide.")
      }
    }
  }

  def servirClient(machine: Machine): Unit = {
    println("Sélectionnez votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70")
    var choix = readInt()

    var prixBoisson = 0.0
    var cafeNecessaire = 0
    var laitNecessaire = 0
    choix match {
      case 1 =>
        prixBoisson = 2.00
        cafeNecessaire = 8
        laitNecessaire = 0
      case 2 =>
        prixBoisson = 2.50
        cafeNecessaire = 6
        laitNecessaire = 100
      case 3 =>
        println("Choisissez la taille :")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")
        val taille = readInt()
        taille match {
          case 1 =>
            prixBoisson = 2.70
            cafeNecessaire = 6
            laitNecessaire = 120
          case 2 =>
            prixBoisson = 3.20
            cafeNecessaire = 8
            laitNecessaire = 150
          case 3 =>
            prixBoisson = 3.70
            cafeNecessaire = 12
            laitNecessaire = 200
          case _ =>
            println("Taille invalide.")
            return
        }
      case _ =>
        println("Choix invalide.")
        return
    }

    println("Ajoutez du sucre :")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    val choixSucre = readInt()
    var sucreNecessaire = 0
    var prixSucre = 0.0
    choixSucre match {
      case 2 =>
        sucreNecessaire = 5
        prixSucre = 0.10
      case 3 =>
        sucreNecessaire = 10
        prixSucre = 0.20
      case 4 =>
        sucreNecessaire = 15
        prixSucre = 0.30
      case _ =>
    }

    println("Ajoutez du lait supplémentaire :")
    println("1) Non")
    println("2) 50ml - CHF 0.05")
    println("3) 100ml - CHF 0.10")
    println("4) 150ml - CHF 0.15")
    val choixLait = readInt()
    var laitSupplementaire = 0
    var prixLaitSupplementaire = 0.0
    choixLait match {
      case 2 =>
        laitSupplementaire = 50
        prixLaitSupplementaire = 0.05
      case 3 =>
        laitSupplementaire = 100
        prixLaitSupplementaire = 0.10
      case 4 =>
        laitSupplementaire = 150
        prixLaitSupplementaire = 0.15
      case _ =>
    }

    val prixTotal = prixBoisson + prixSucre + prixLaitSupplementaire

    if (machine.retirerIngredient("cafe", cafeNecessaire) &&
      machine.retirerIngredient("sucre", sucreNecessaire) &&
      machine.retirerIngredient("lait", laitNecessaire + laitSupplementaire)) {
      val codeTwint = genererCodeTwint()
      println(f"Prix total : CHF $prixTotal%.2f")
      println(s"Code Twint : $codeTwint")
      println("En attente de paiement...\n")
      Thread.sleep(3000)
      println("Paiement confirmé.")
      println("Votre boisson est prête ! Bonne dégustation.")
    } else {
      println("Échec de la préparation. Vérifiez les stocks.")
    }
  }

  def modeAdmin(machine: Machine): Unit = {
    println("Mode Admin")
    println("Entrez le PIN :")
    val pin = readLine()
    if (pin == machine.codePIN) {
      println("1) Modifier le PIN")
      println("2) Réapprovisionner les stocks")
      val choix = readInt()
      choix match {
        case 1 =>
          println("Nouveau PIN :")
          machine.codePIN = readLine()
          println("PIN mis à jour.")
        case 2 =>
          println("Ajout de lait (ml) :")
          machine.ajouterIngredient("lait", readInt())
          println("Ajout de sucre (g) :")
          machine.ajouterIngredient("sucre", readInt())
          println("Ajout de café (g) :")
          machine.ajouterIngredient("cafe", readInt())
        case _ => println("Choix invalide.")
      }
    } else {
      println("PIN incorrect.")
    }
  }

  def chargerCSV(nomFichier: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    val fichier = scala.io.Source.fromFile(nomFichier)
    var premiereLigne = true
    for (ligne <- fichier.getLines()) {
      if (!premiereLigne) {
        val elements = extraireElementsCSV(ligne)
        if (elements.length == 4) {
          val id = machines.size + 1
          val pincode = elements(0)
          val lait = convertir(elements(1))
          val sucre = convertir(elements(2))
          val cafe = convertir(elements(3))
          if (lait >= 0 && sucre >= 0 && cafe >= 0) {
            machines += new Machine(id, pincode, lait, sucre, cafe)
          } else {
            println(s"Données invalides ignorées : $ligne")
          }
        } else {
          println(s"Ligne mal formatée ignorée : $ligne")
        }
      } else {
        premiereLigne = false
      }
    }
    fichier.close()
    machines
  }

  def sauvegarderCSV(nomFichier: String, machines: ArrayBuffer[Machine]): Unit = {
    val writer = new java.io.PrintWriter(nomFichier)
    writer.println("PINCODE,LAIT,SUCRE,CAFE")
    for (machine <- machines) {
      writer.println(machine.codePIN + "," + machine.lait + "," + machine.sucre + "," + machine.cafe)
    }
    writer.close()
    println("Sauvegarde effectuée avec succès.")
  }

  def extraireElementsCSV(ligne: String): Array[String] = {
    val elements = ArrayBuffer[String]()
    var element = ""
    for (x <- ligne) {
      if (x == ',') {
        elements.append(element)
        element = ""
      } else {
        element += x
      }
    }
    elements.append(element)
    elements.toArray
  }

  def convertir(s: String): Int = {
    var resultat = 0
    var valide = true
    for (c <- s) {
      if (c >= '0' && c <= '9') {
        resultat = resultat * 10 + (c - '0')
      } else {
        valide = false
      }
    }
    if (valide) resultat else -1
  }

  def genererCodeTwint(): String = {
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var code = ""
    for (_ <- 1 to 5) {
      val caracteresrandom = (Math.random() * caracteres.length).toInt
      code += caracteres(caracteresrandom)
    }
    code
  }
}
