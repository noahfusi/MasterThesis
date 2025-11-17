
import scala.util.Random
import math._
import scala.io.StdIn._

object Main {

  var boisson = 0
  var ajoutsucre = 0
  var ajoutlait = 0
  var choixboisson = "0"
  var niveausucre = "0"
  var laitsup = "0"
  var prixtotal = 0.00
  var prix = 0.00
  var prixsucre = 0.00
  var prixlait = 0.00
  var choix = 0
  var code = "0"
  var pin = "0"
  var boucle = true
  var Quantitecafe = 0
  var Quantitesucre = 0
  var Quantitelait = 0
  var tentative = true
  val nbMachines = 5
  val machinePins = Array("434343","434343","434343","434343","434343")
  val coffeeStocks = Array(50,50,50,50,50)
  val sugarStocks = Array(30,30,30,30,30)
  val milkStocks = Array(500,500,500,500,500)






  def main(args: Array[String]): Unit = {



    while (boucle) {

      boucle = true

      var machine = readLine("Machine sélectionnée (1-5) ").toInt

      if ((machine != 1) && (machine != 2) && (machine != 3) && (machine != 4) && (machine != 5)) {
        println("Erreur choisissez une valeur valide.")
        println("Machine sélectionnée (1-5)")
        machine = readInt()

        if (machine == 1){
          machine = 0
        }else if (machine == 2){
          machine = 1
        }else if (machine == 3){
          machine = 2
        }else if (machine == 4){
          machine = 3
        }else if (machine == 5){
          machine = 4
        }


      }else {

        if (machine == 1) {
          machine = 0
        } else if (machine == 2) {
          machine = 1
        } else if (machine == 3) {
          machine = 2
        } else if (machine == 4) {
          machine = 3
        } else if (machine == 5) {
          machine = 4
        }


      }

      println("         Nospresso Café")
      println(" Veuillez sélectionner votre mode : ")
      println(" 1) Client")
      println(" 2) Admin")
      println(" 3) Quitter")
      println(" > ")

      choix = readLine().toInt


      while ((choix != 1) && (choix != 2) && (choix != 3)) {
        println("         Nospresso Café")
        println(" Veuillez sélectionner votre mode : ")
        println(" 1) Client")
        println(" 2) Admin")
        println(" 3) Quitter")
        println(" > ")
        choix = readLine().toInt

      }


      if (choix == 1) {


        serveClient( machine, coffeeStocks, sugarStocks, milkStocks)


      } else if (choix == 2) {


        pin = readLine("Entrez le code PIN : ")


        validatePin(machine,machinePins)



        if (tentative == false ){

          boucle = false

        }else {

          println("Accès accordé à la Machine " + (machine + 1) + "\n")

          var question = readLine("Voulez-vous changer le code Pin (oui ou non) ? \n")
          while ((question!= "oui") && (question != "non")) {

            question = readLine("Voulez-vous changer le code Pin (oui ou non) ? \n")

          }
          if (question == "oui") {

            println("Mise à jour du code PIN pour la Machine " + (machine + 1))

            updatePin(machine, machinePins)

          } else {
            println()
            restockMachine(machine, coffeeStocks, sugarStocks, milkStocks)
          }
        }
      } else {
        boucle = false
      }

    }

  }


  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {

    println("Veuillez s'électionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    println("> ")
    boisson = readLine().toInt

    while ((boisson != 1) && (boisson != 2) && (boisson != 3) ) {
      println("Veuillez entrer une valeur valide.\n")
      println("Veuillez s'électionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      println("> ")
      boisson = readLine().toInt
    }

    if (boisson == 3) {
      println("4) CHF 2.70 (Petit)")
      println("5) CHF 3.20 (Moyen)")
      println("6) CHF 3.70 (Grand) ")
      println("> ")
      boisson = readLine().toInt

      while ((boisson != 4) && (boisson != 5) && (boisson != 6) ) {
        println("Veuillez entrer une valeur valide.\n")
        println("4) CHF 2.70 (Petit)")
        println("5) CHF 3.20 (Moyen)")
        println("6) CHF 3.70 (Grand) ")
        println("> ")
        boisson = readLine().toInt
      }


      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      println("> ")
      ajoutsucre = readLine().toInt

      while ((ajoutsucre != 1) && (ajoutsucre != 2) && (ajoutsucre != 3) && (ajoutsucre != 4) ) {
        println("Veuillez entrer une valeur valide.\n")
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        println("> ")
        ajoutsucre = readLine().toInt
      }


      println("Souhaitez-vous ajouter du lait en supplément ?")
      println("(Disponible uniquement pour Cappuccino et Latte)")
      println("1) Oui")
      println("2) Non")
      println("> ")
      ajoutlait = readLine().toInt

      while ((ajoutlait != 1) && (ajoutlait != 2) ) {
        println("Veuillez entrer une valeur valide.\n")
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("(Disponible uniquement pour Cappuccino et Latte)")
        println("1) Oui")
        println("2) Non")
        println("> ")
        ajoutlait = readLine().toInt
      }

      if (ajoutlait == 1) {

        println("Combien de dose ? 1, 2 ou 3")
        println(">")

        ajoutlait = readLine().toInt

      } else {
        ajoutlait = 0
      }

      if (boisson == 1) {
        choixboisson = "Expresso"
      } else if (boisson == 2) {
        choixboisson = "Cappuccino"
      } else {
        choixboisson = "Latte"
      }

      if (ajoutsucre == 1) {
        niveausucre = "Sans sucre"
      } else if (ajoutsucre == 2) {
        niveausucre = "Peu (5g)"
      } else if (ajoutsucre == 3) {
        niveausucre = "Moyen (10g)"
      } else {
        niveausucre = "Beaucoup (15g)"
      }

      if (ajoutlait == 1) {
        laitsup = "Une dose"
      } else if (ajoutlait == 2) {
        laitsup = "Deux doses"
      } else if (ajoutlait == 3) {
        laitsup = "Trois doses"
      } else {
        laitsup = "Non"
      }

      if (boisson == 4) {
        //petit//

        Quantitecafe = (Quantitecafe - 6)

        if (ajoutsucre == 1) {
          Quantitesucre = (Quantitesucre - 0)
        } else if (ajoutsucre == 2) {
          Quantitesucre = (Quantitesucre - 5)
        } else if (ajoutsucre == 3) {
          Quantitesucre = (Quantitesucre - 10)
        } else {
          Quantitesucre = (Quantitesucre - 15)
        }

        if (ajoutlait == 1) {
          Quantitelait = (Quantitelait - 170)
        } else if (ajoutlait == 2) {
          Quantitelait = (Quantitelait - 220)
        } else if (ajoutlait == 3) {
          Quantitelait = (Quantitelait - 270)
        } else {
          Quantitelait = (Quantitelait - 120)
        }

      } else if (boisson == 5) {
        //moyen//

        Quantitecafe = (Quantitecafe - 8)

        if (ajoutsucre == 1) {
          Quantitesucre = (Quantitesucre - 0)
        } else if (ajoutsucre == 2) {
          Quantitesucre = (Quantitesucre - 5)
        } else if (ajoutsucre == 3) {
          Quantitesucre = (Quantitesucre - 10)
        } else {
          Quantitesucre = (Quantitesucre - 15)
        }

        if (ajoutlait == 1) {
          Quantitelait = (Quantitelait - 200)
        } else if (ajoutlait == 2) {
          Quantitelait = (Quantitelait - 250)
        } else if (ajoutlait == 3) {
          Quantitelait = (Quantitelait - 300)
        } else {
          Quantitelait = (Quantitelait - 150)
        }

      } else {
        //grand//

        Quantitecafe = (Quantitecafe - 12)

        if (ajoutsucre == 1) { //sans sucre//
          Quantitesucre = (Quantitesucre - 0)
        } else if (ajoutsucre == 2) {
          Quantitesucre = (Quantitesucre - 5)
        } else if (ajoutsucre == 3) {
          Quantitesucre = (Quantitesucre - 10)
        } else {
          Quantitesucre = (Quantitesucre - 15)
        }

        if (ajoutlait == 1) {
          Quantitelait = (Quantitelait - 250)
        } else if (ajoutlait == 2) {
          Quantitelait = (Quantitelait - 300)
        } else if (ajoutlait == 3) {
          Quantitelait = (Quantitelait - 350)
        } else {
          Quantitelait = (Quantitelait - 200)
        }

      }

      if ((-Quantitesucre > sugarStocks(machineId))) {
        println("Boisson s'électionnée : " + choixboisson)
        println("Niveau de sucre : " + niveausucre + "\n")
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre machine. \n")

        Quantitesucre = 0
        Quantitelait = 0
        Quantitecafe = 0

      } else if ((-Quantitelait > milkStocks(machineId))) {
        println("Boisson s'électionnée : " + choixboisson)
        println("Niveau de sucre : " + niveausucre)
        println("Lait supplémentaire: " + laitsup + "\n")
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre machine. \n")

        Quantitelait = 0
        Quantitecafe = 0
        Quantitesucre = 0

      } else if ((-Quantitecafe > coffeeStocks(machineId))) {

        println("Boisson s'électionnée : " + choixboisson)
        println("Niveau de sucre : " + niveausucre + "\n")
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre machine. \n")

        Quantitesucre = 0
        Quantitelait = 0
        Quantitecafe = 0

      } else {

        Quantitesucre = 0
        Quantitelait = 0
        Quantitecafe = 0

        if (boisson == 4) {
          //petit//

          coffeeStocks(machineId ) = (coffeeStocks(machineId ) - 6)

          if (ajoutsucre == 1) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 0)
          } else if (ajoutsucre == 2) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 5)
          } else if (ajoutsucre == 3) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 10)
          } else {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 15)
          }

          if (ajoutlait == 1) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 170)
          } else if (ajoutlait == 2) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 220)
          } else if (ajoutlait == 3) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 270)
          } else {
            milkStocks(machineId ) = (milkStocks(machineId ) - 120)
          }

        } else if (boisson == 5) {
          //moyen//

          coffeeStocks(machineId ) = (coffeeStocks(machineId ) - 8)

          if (ajoutsucre == 1) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 0)
          } else if (ajoutsucre == 2) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 5)
          } else if (ajoutsucre == 3) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 10)
          } else {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 15)
          }

          if (ajoutlait == 1) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 200)
          } else if (ajoutlait == 2) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 250)
          } else if (ajoutlait == 3) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 300)
          } else {
            milkStocks(machineId ) = (milkStocks(machineId ) - 150)
          }

        } else {
          //grand//

          coffeeStocks(machineId ) = (coffeeStocks(machineId ) - 12)

          if (ajoutsucre == 1) { //sans sucre//
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 0)
          } else if (ajoutsucre == 2) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 5)
          } else if (ajoutsucre == 3) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 10)
          } else {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 15)
          }

          if (ajoutlait == 1) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 250)
          } else if (ajoutlait == 2) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 300)
          } else if (ajoutlait == 3) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 350)
          } else {
            milkStocks(machineId ) = (milkStocks(machineId ) - 200)
          }

        }


        if (boisson == 4) {
          prix = 2.70
        } else if (boisson == 5) {
          prix = 3.20
        } else {
          prix = 3.70
        }

        if (ajoutsucre == 1) {
          prixsucre = 0.00
        } else if (ajoutsucre == 2) {
          prixsucre = 0.10
        } else if (ajoutsucre == 3) {
          prixsucre = 0.20
        } else {
          prixsucre = 0.30
        }

        if (ajoutlait == 1) {
          prixlait = 0.05
        } else if (ajoutlait == 2) {
          prixlait = 0.10
        } else if (ajoutlait == 3) {
          prixlait = 0.15
        } else {
          prixlait = 0.0
        }

        prixtotal = (prix + prixlait + prixsucre)


        println("Boisson s'électionnée : " + choixboisson)
        println("Niveau de sucre : " + niveausucre)
        println("Lait supplémentaire: " + laitsup)
        printf("Prix total : CHF %.2f +  CHF %.2f + CHF %.2f = CHF %.2f \n", prix, prixsucre, prixlait, prixtotal)
        println()
        println("Veuillez payer en utilisant Twint")
        code = Random.alphanumeric.take(5).mkString("")
        println("Votre code de paiement est : " + code)
        println("(En attente de validation du paiement...)\n")
        Thread.sleep(3000)

        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("[...]")
        println("Votre " + choixboisson + " est prêt ! Bonne dégustation ! \n")

      }

    } else { //boisson 1 et 2 //

      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      println("> ")
      ajoutsucre = readLine().toInt

      while ((ajoutsucre != 1) && (ajoutsucre != 2) && (ajoutsucre != 3) && (ajoutsucre != 4)) {
        println("Veuillez entrer une valeur valide.\n")
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        println("> ")
        ajoutsucre = readLine().toInt
      }

      if (boisson == 2) {

        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("(Disponible uniquement pour Cappuccino et Latte)")
        println("1) Oui")
        println("2) Non")
        println("> ")
        ajoutlait = readLine().toInt

        while ((ajoutlait != 1) && (ajoutlait != 2) ) {
          println("Veuillez entrer une valeur valide.\n")
          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("(Disponible uniquement pour Cappuccino et Latte)")
          println("1) Oui")
          println("2) Non")
          println("> ")
          ajoutlait = readLine().toInt
        }

        if (ajoutlait == 1) {

          println("Combien de dose ? 1, 2 ou 3")
          println(">")

          ajoutlait = readLine().toInt

        } else {
          ajoutlait = 0
        }
      }

      if (boisson == 1) {
        choixboisson = "Expresso"
      } else if (boisson == 2) {
        choixboisson = "Cappuccino"
      } else {
        choixboisson = "Latte"
      }

      if (ajoutsucre == 1) {
        niveausucre = "Sans sucre"
      } else if (ajoutsucre == 2) {
        niveausucre = "Peu (5g)"
      } else if (ajoutsucre == 3) {
        niveausucre = "Moyen (10g)"
      } else {
        niveausucre = "Beaucoup (15g)"
      }

      if (ajoutlait == 1) {
        laitsup = "Une dose"
      } else if (ajoutlait == 2) {
        laitsup = "Deux doses"
      } else if (ajoutlait == 3) {
        laitsup = "Trois doses"
      } else {
        laitsup = "Non"
      }


      if (boisson == 1) {

        Quantitecafe = (Quantitecafe - 8)

        if (ajoutsucre == 1) {
          Quantitesucre = ( Quantitesucre - 0)
        } else if (ajoutsucre == 2) {
          Quantitesucre = (Quantitesucre - 5)
        } else if (ajoutsucre == 3) {
          Quantitesucre = (Quantitesucre - 10)
        } else {
          Quantitesucre = (Quantitesucre - 15)
        }

        if (ajoutlait == 1) {
          Quantitelait = (Quantitelait - 50)
        } else if (ajoutlait == 2) {
          Quantitelait = (Quantitelait - 100)
        } else if (ajoutlait == 3) {
          Quantitelait = (Quantitelait - 150)
        } else {
          Quantitelait = (Quantitelait - 0)
        }


      } else {

        Quantitecafe = (Quantitecafe - 6)

        if (ajoutsucre == 1) {
          Quantitesucre = (Quantitesucre - 0)
        } else if (ajoutsucre == 2) {
          Quantitesucre = (Quantitesucre - 5)
        } else if (ajoutsucre == 3) {
          Quantitesucre = (Quantitesucre - 10)
        } else {
          Quantitesucre = (Quantitesucre - 15)
        }

        if (ajoutlait == 1) {
          Quantitelait = (Quantitelait - 150)
        } else if (ajoutlait == 2) {
          Quantitelait = (Quantitelait - 200)
        } else if (ajoutlait == 3) {
          Quantitelait = (Quantitelait - 250)
        } else {
          Quantitelait = (Quantitelait - 100)
        }

      }



      if (-Quantitesucre > sugarStocks(machineId)) {
        println("Boisson s'électionnée : " + choixboisson)
        println("Niveau de sucre : " + niveausucre + "\n")
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre machine. \n")

        Quantitesucre = 0
        Quantitelait = 0
        Quantitecafe = 0


      } else if (-Quantitelait > milkStocks(machineId)) {
        println("Boisson s'électionnée : " + choixboisson)
        println("Niveau de sucre : " + niveausucre)
        println("Lait supplémentaire: " + laitsup + "\n")
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre machine. \n")

        Quantitesucre = 0
        Quantitelait = 0
        Quantitecafe = 0

      } else if (-Quantitecafe > coffeeStocks(machineId)) {

        println("Boisson s'électionnée : " + choixboisson)
        println("Niveau de sucre : " + niveausucre + "\n")
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre machine. \n")

        Quantitesucre = 0
        Quantitelait = 0
        Quantitecafe = 0

      } else {

        Quantitesucre = 0
        Quantitelait = 0
        Quantitecafe = 0


        if (boisson == 1) {

          coffeeStocks(machineId ) = (coffeeStocks(machineId ) - 8)

          if (ajoutsucre == 1) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 0)
          } else if (ajoutsucre == 2) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 5)
          } else if (ajoutsucre == 3) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 10)
          } else {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 15)
          }

          if (ajoutlait == 1) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 50)
          } else if (ajoutlait == 2) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 100)
          } else if (ajoutlait == 3) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 150)
          } else {
            milkStocks(machineId ) = (milkStocks(machineId ) - 0)
          }


        } else {

          coffeeStocks(machineId ) = (coffeeStocks(machineId ) - 6)

          if (ajoutsucre == 1) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 0)
          } else if (ajoutsucre == 2) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 5)
          } else if (ajoutsucre == 3) {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 10)
          } else {
            sugarStocks(machineId ) = (sugarStocks(machineId ) - 15)
          }

          if (ajoutlait == 1) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 150)
          } else if (ajoutlait == 2) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 200)
          } else if (ajoutlait == 3) {
            milkStocks(machineId ) = (milkStocks(machineId ) - 250)
          } else {
            milkStocks(machineId ) = (milkStocks(machineId ) - 100)
          }

        }


        if (boisson == 1) {
          prix = 2.00
          if (ajoutsucre == 1) {
            prixsucre = 0.00
          } else if (ajoutsucre == 2) {
            prixsucre = 0.10
          } else if (ajoutsucre == 3) {
            prixsucre = 0.20
          } else {
            prixsucre = 0.30
          }

        } else {
          prix = 2.50
          if (ajoutsucre == 1) {
            prixsucre = 0.00
          } else if (ajoutsucre == 2) {
            prixsucre = 0.10
          } else if (ajoutsucre == 3) {
            prixsucre = 0.20
          } else {
            prixsucre = 0.30
          }

        }

        if (ajoutlait == 1) {
          prixlait = 0.05
        } else if (ajoutlait == 2) {
          prixlait = 0.10
        } else if (ajoutlait == 3) {
          prixlait = 0.15
        } else {
          prixlait = 0.00
        }

        prixtotal = (prix + prixlait + prixsucre)

        println("Boisson s'électionnée : " + choixboisson)
        println("Niveau de sucre : " + niveausucre)
        println("Lait supplémentaire: " + laitsup)
        printf("Prix total : CHF %.2f +  CHF %.2f + CHF %.2f = CHF %.2f \n", prix, prixsucre, prixlait, prixtotal)
        println()
        println("Veuillez payer en utilisant Twint")
        code = Random.alphanumeric.take(5).mkString("")
        println("Votre code de paiement est : " + code)
        println("(En attente de validation du paiement...)\n")
        Thread.sleep(3000)

        println("Merci ! Votre paiement a été accepté.")
        println("Préparation de votre boisson...")
        println("[...]")
        println("Votre " + choixboisson + " est prêt ! Bonne dégustation ! \n")

      }


    }
    return true
  }


  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {


    var lait = milkStocks(machineId).toDouble
    lait = (lait / 1000)

    println("Stocks :")
    println("Poudre de café : " + coffeeStocks(machineId ) + " g")
    printf("Lait           : %.3f L \n", lait)
    println("Sucre          : " + sugarStocks(machineId ) + " g \n")

    println("Réapprovisionnement des stocks...")
    println("Ajout :")
    println("Poudre de café : ")
    coffeeStocks(machineId ) += readLine().toInt
    println("Lait           : ")

    lait += readLine().toDouble
    lait = (lait * 1000)
    var newlait = lait.toInt
    milkStocks(machineId ) = newlait

    println("Sucre          : ")
    sugarStocks(machineId ) += readLine().toInt

    lait = milkStocks(machineId).toDouble
    lait = (lait / 1000)

    println()
    println("Poudre de café : " + coffeeStocks(machineId ) + " g")
    printf("Lait           : %.3f L \n", lait)
    println("Sucre          : " + sugarStocks(machineId ) + " g \n")

    println("Niveaux de stock mis à jour.")
    println("Retour au menu principal... \n")


  }


  def validatePin(machineId: Int, machinePins: Array[String]) : Boolean = {

    if (pin != machinePins(machineId )) {
      println("Code PIN incorrect. 2 tentatives restantes.")
      pin = readLine("Entrez le code PIN : ****** ")
      if (pin != machinePins(machineId )) {
        println("Code PIN incorrect. 1 tentatives restantes.")
        pin = readLine("Entrez le code PIN : ****** ")
        if (pin != machinePins(machineId )) {
          println("Code PIN incorrect. 0 tentatives restantes.")
          println("Trop de tentatives échouées. Fin du programme.")
          tentative = false
        }
      }
      return false
    } else {
      tentative = true
      return true
    }

   }


  def updatePin(machineId: Int,machinePins: Array[String]): Unit = {



      var newcode = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      while (newcode.length() != 6){

        newcode = readLine("Entrez un nouveau code PIN à 6 chiffres > \n")

      }
        machinePins( machineId ) = newcode
        pin = newcode

        println("Le code PIN a été mis à jour avec succès.")
        println("Retour au menu principal...\n")







  }
}