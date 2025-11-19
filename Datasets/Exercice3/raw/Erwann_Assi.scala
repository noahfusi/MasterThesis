
import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer

object Main  {

  class Machine(val id: Int, var codePIN: String, var lait: Int, var sucre: Int, var cafe: Int) {

    def ajouterStock(ingredient: String, quantite: Int): Unit = {
      if (quantite <= 0) {
        println("Quantité invalide.")
      } else {
        ingredient match {
          case "lait" =>
            lait += quantite
            println(s"Lait ajouté : $quantite ml. Nouveau stock : $lait ml.")
          case "sucre" =>
            sucre += quantite
            println(s"Sucre ajouté : $quantite g. Nouveau stock : $sucre g.")
          case "cafe" =>
            cafe += quantite
            println(s"Café ajouté : $quantite g. Nouveau stock : $cafe g.")
          case _ => println("Ingrédient inconnu.")
        }
      }
    }

    def retirerStock(ingredient: String, quantite: Int): Boolean = {
      if (quantite <= 0) {
        println("Quantité invalide.")
        false
      } else {
        ingredient match {
          case "lait" if lait >= quantite =>
            lait -= quantite
            println(s"Lait utilisé : $quantite ml. Stock restant : $lait ml.")
            true
          case "sucre" if sucre >= quantite =>
            sucre -= quantite
            println(s"Sucre utilisé : $quantite g. Stock restant : $sucre g.")
            true
          case "cafe" if cafe >= quantite =>
            cafe -= quantite
            println(s"Café utilisé : $quantite g. Stock restant : $cafe g.")
            true
          case "lait" =>
            println(s"Pas assez de lait : $lait ml disponibles, $quantite ml requis.")
            false
          case "sucre" =>
            println(s"Pas assez de sucre : $sucre g disponibles, $quantite g requis.")
            false
          case "cafe" =>
            println(s"Pas assez de café : $cafe g disponibles, $quantite g requis.")
            false
          case _ =>
            println("Ingrédient inconnu.")
            false
        }
      }
    }
  }

  def chargerFichier(nomFichier: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val fichier = scala.io.Source.fromFile(nomFichier)
      var premiereLigneIgnoree = false
      for (ligne <- fichier.getLines()) {
        if (premiereLigneIgnoree) {
          val donnees = separerLigneCSV(ligne)
          if (donnees.length == 4) {
            val id = machines.size + 1
            val code = donnees(0)
            val lait = convertirEnNombre(donnees(1))
            val sucre = convertirEnNombre(donnees(2))
            val cafe = convertirEnNombre(donnees(3))

            if (lait >= 0 && sucre >= 0 && cafe >= 0) {
              machines += new Machine(id, code, lait, sucre, cafe)
            } else {
              println(s"Données invalides ignorées : $ligne")
            }
          } else {
            println(s"Ligne mal formatée ignorée : $ligne")
          }
        } else {
          premiereLigneIgnoree = true
        }
      }
      fichier.close()
    } catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Le fichier n'a pas été trouvé.")
        sys.exit(1)
    }
    machines
  }

  def sauvegarderFichier(nomFichier: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new java.io.PrintWriter(nomFichier)
      writer.println("CODE,LAIT,SUCRE,CAFE")
      for (machine <- machines) {
        writer.println(s"${machine.codePIN},${machine.lait},${machine.sucre},${machine.cafe}")
      }
      writer.close()
      println("Données sauvegardées avec succès.")
    } catch {
      case _: java.io.IOException => println("Erreur lors de la sauvegarde des données.")
    }
  }

  def separerLigneCSV(ligne: String): Array[String] = {
    val elements = ArrayBuffer[String]()
    var contenu = ""
    for (caractere <- ligne) {
      if (caractere == ',') {
        elements.append(contenu)
        contenu = ""
      } else {
        contenu += caractere
      }
    }
    elements.append(contenu)
    elements.toArray
  }

  def convertirEnNombre(chaine: String): Int = {
    var resultat = 0
    var valide = true
    for (caractere <- chaine) {
      if (caractere >= '0' && caractere <= '9') {
        resultat = resultat * 10 + (caractere - '0')
      } else {
        valide = false
      }
    }
    if (valide) resultat else -1
  }

  def genererCodePaiement(): String = {
    val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var code = ""
    for (_ <- 1 to 6) {
      val indexAleatoire = (Math.random() * caracteres.length).toInt
      code += caracteres(indexAleatoire)
    }
    code
  }

  def interactionClient(machine: Machine): Unit = {
    var choixBoisson = 0
    while (choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3) {
      println("Choisissez une boisson :")
      println("1) Expresso - 2 CHF")
      println("2) Cappuccino - 2.5 CHF")
      println("3) Latte")

      choixBoisson = readInt()
      if (choixBoisson != 1 && choixBoisson != 2 && choixBoisson != 3) {
        println("Choix invalide. Veuillez réessayer.")
      }
    }

    var lait = 0
    var cafe = 0
    var prixBase = 0.0

    choixBoisson match {
      case 1 =>
        lait = 0
        cafe = 8
        prixBase = 2.0
      case 2 =>
        lait = 100
        cafe = 6
        prixBase = 2.5
      case 3 =>
        var tailleLatte = 0
        while (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
          println("Choisissez la taille du Latte :")
          println("1) Petit - 2.7 CHF")
          println("2) Moyen - 3.2 CHF")
          println("3) Grand - 3.7 CHF")

          tailleLatte = readInt()
          if (tailleLatte != 1 && tailleLatte != 2 && tailleLatte != 3) {
            println("Choix invalide. Veuillez réessayer.")
          }
        }

        tailleLatte match {
          case 1 =>
            lait = 120
            cafe = 6
            prixBase = 2.7
          case 2 =>
            lait = 150
            cafe = 8
            prixBase = 3.2
          case 3 =>
            lait = 200
            cafe = 12
            prixBase = 3.7
        }
    }

    var choixSucre = 0
    while (choixSucre != 1 && choixSucre != 2 && choixSucre != 3 && choixSucre != 4) {
      println("Ajoutez du sucre :")
      println("1) Sans sucre")
      println("2) Peu (5g) - 0.10 CHF")
      println("3) Moyen (10g) - 0.20 CHF")
      println("4) Beaucoup (15g) - 0.30 CHF")

      choixSucre = readInt()
      if (choixSucre != 1 && choixSucre != 2 && choixSucre != 3 && choixSucre != 4) {
        println("Choix invalide. Veuillez réessayer.")
      }
    }

    var sucre = 0
    var prixSucre = 0.0

    choixSucre match {
      case 2 =>
        sucre = 5
        prixSucre = 0.10
      case 3 =>
        sucre = 10
        prixSucre = 0.20
      case 4 =>
        sucre = 15
        prixSucre = 0.30
      case _ =>
        sucre = 0
        prixSucre = 0.0
    }

    var choixLait = 0
    while (choixLait != 1 && choixLait != 2 && choixLait != 3 && choixLait != 4) {
      println("Ajoutez du lait supplémentaire :")
      println("1) Non")
      println("2) 50ml - 0.05 CHF")
      println("3) 100ml - 0.10 CHF")
      println("4) 150ml - 0.15 CHF")

      choixLait = readInt()
      if (choixLait != 1 && choixLait != 2 && choixLait != 3 && choixLait != 4) {
        println("Choix invalide. Veuillez réessayer.")
      }
    }

    var laitSupp = 0
    var prixLaitSupp = 0.0

    choixLait match {
      case 2 =>
        laitSupp = 50
        prixLaitSupp = 0.05
      case 3 =>
        laitSupp = 100
        prixLaitSupp = 0.10
      case 4 =>
        laitSupp = 150
        prixLaitSupp = 0.15
      case _ =>
        laitSupp = 0
        prixLaitSupp = 0.0
    }

    val prixTotal = prixBase + prixSucre + prixLaitSupp

    if (machine.retirerStock("cafe", cafe) && machine.retirerStock("lait", lait + laitSupp) && machine.retirerStock("sucre", sucre)) {
      val code = genererCodePaiement()
      println(f"Prix total : $prixTotal%.2f CHF. Code de paiement : $code")
      println("Votre boisson est prête. Merci !")
    } else {
      println("Préparation impossible. Vérifiez les stocks.")
    }
  }

  def interactionAdmin(machine: Machine): Unit = {
    println("Mode administrateur. Entrez le code :")
    var codeSaisi = ""

    do {
      codeSaisi = readLine()
      if (codeSaisi != machine.codePIN) {
        println("Code incorrect. Veuillez réessayer.")
      }
    } while (codeSaisi != machine.codePIN)

    println("1) Modifier le code")
    println("2) Réapprovisionner les stocks")

    var choixAdmin = 0
    while (choixAdmin != 1 && choixAdmin != 2) {
      choixAdmin = readInt()
      if (choixAdmin != 1 && choixAdmin != 2) {
        println("Choix invalide. Veuillez réessayer.")
      }
    }

    choixAdmin match {
      case 1 =>
        println("Nouveau code :")
        var nouveauCode = ""
        do {
          nouveauCode = readLine()
          if (!(nouveauCode.length == 6 && nouveauCode.forall(c => c >= '0' && c <= '9'))) {
            println("Le code doit contenir exactement 6 chiffres. Veuillez réessayer.")
          }
        } while (!(nouveauCode.length == 6 && nouveauCode.forall(c => c >= '0' && c <= '9')))

        machine.codePIN = nouveauCode
        println("Code modifié avec succès.")

      case 2 =>
        println("Ajout de lait (ml) :")
        var lait = -1
        while (lait < 0) {
          lait = readInt()
          if (lait < 0) println("Veuillez entrer une quantité valide.")
        }
        machine.ajouterStock("lait", lait)

        println("Ajout de sucre (g) :")
        var sucre = -1
        while (sucre < 0) {
          sucre = readInt()
          if (sucre < 0) println("Veuillez entrer une quantité valide.")
        }
        machine.ajouterStock("sucre", sucre)

        println("Ajout de café (g) :")
        var cafe = -1
        while (cafe < 0) {
          cafe = readInt()
          if (cafe < 0) println("Veuillez entrer une quantité valide.")
        }
        machine.ajouterStock("cafe", cafe)

      case _ => println("Choix invalide.")
    }
  }


  def main(args: Array[String]): Unit = {
    val machines = chargerFichier("machines.csv")
    println(s"${machines.size} machine(s) chargée(s).")

    var enCours = true
    while (enCours) {
      println("1) Utiliser une machine")
      println("2) Quitter")
      val choixPrincipal = readInt()

      choixPrincipal match {
        case 1 =>
          println("Choisissez une machine :")
          val idMachine = readInt() - 1

          if (idMachine >= 0 && idMachine < machines.size) {
            val machine = machines(idMachine)
            println("1) Client")
            println("2) Administrateur")
            readInt() match {
              case 1 => interactionClient(machine)
              case 2 => interactionAdmin(machine)
              case _ => println("Choix invalide.")
            }
          } else {
            println("Machine invalide.")
          }
        case 2 =>
          sauvegarderFichier("machines.csv", machines)
          enCours = false
          println("Programme terminé.")
        case _ => println("Choix invalide.")
      }
    }
  }
}
