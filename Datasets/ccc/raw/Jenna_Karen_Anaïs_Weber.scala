
import java.io.FileNotFoundException
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.io.StdIn._
import scala.util.Random

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


  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {


    def addIngredient(ingredient: String, amount: Int): Unit = {

      if (ingredient == "coffee") {
        coffee += amount
      } else if (ingredient == "milk") {
        milk += amount
      } else if (ingredient == "sugar") {
        sugar += amount
      }

    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {


      if (ingredient == "coffee") {
        if (coffee - amount < 0) {
          return false
        }
        coffee -= amount
      } else if (ingredient == "milk") {
        if (milk - amount < 0) {
          return false
        }
        milk -= amount
      } else if (ingredient == "sugar") {
        if (sugar - amount < 0) {
          return false
        }
        sugar -= amount
      }

      return true
    }
  }

  val machines: ArrayBuffer[Machine] = ArrayBuffer[Machine]()


  def loadcsv(filename: String): Unit = {
    try {
      val machineSource = Source.fromFile(filename)
      var id: Int = 1

      for (line <- machineSource.getLines.drop(1)) {

        val pincode: String = line.split(",")(0)
        val milk = line.split(",")(1).toInt
        val sugar = line.split(",")(2).toInt
        val coffee = line.split(",")(3).toInt

        val instance_machine = new Machine(id, pincode, milk, sugar, coffee)
        machines += instance_machine
        id += 1
      }
      machineSource.close()

    } catch {
      case ex: FileNotFoundException =>
        println(" Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
    }
  }


  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val messageErreur: String = "Erreur : Échec de l'écriture dans " + filename + "\n" + "Le fichier peut être verrouillé ou en lecture seule."
    try {
      val writer = new PrintWriter(new File(filename))


      writer.write("PINCODE,MILK,SUGAR,COFFEE\n")


      machines.foreach { machine =>
        writer.write( machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee + "\n")
      }
      writer.close()
    } catch {
      case ex: java.io.IOException =>
        println(messageErreur)
      case ex: java.nio.file.AccessDeniedException =>
        println(messageErreur)
    }
  }


  def main(args: Array[String]): Unit = {


    while (boucle) {

      boucle = true


      println("Chargement des machines depuis machines.csv...\n")
      loadcsv("src/machines.csv")


      var index: Int = -1
      var verification = false


      while (!verification) {
        try {
          index = readLine("Machine sélectionnée > \n").toInt
          index -= 1
          if (index >= 0 && index < machines.length) {
            verification = true
          } else {
            println("Entrez une machine valide entre 0 et "+ (machines.length - 1) + "\n")
          }
        } catch {
          case _: NumberFormatException =>
            println("Entrez une valeur valide.")
        }
      }


      val idMachine: Int = machines(index).id
      val litreLait: Float = machines(index).milk.toFloat / 1000

      println("Machine " + idMachine +" chargée : \n")
      println("  " + "ID: " + idMachine)
      println("  " + "Code PIN: " + machines(index).pincode)
      println("  " + "Lait: " + litreLait + "L")
      println("  " + "Sucre: " + machines(index).sugar)
      println("  " + "Café: " + machines(index).coffee)
      println()
      println( idMachine + " machine(s) chargée(s) avec succès. \n")



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


          serveClient(index: Int)


        } else if (choix == 2) {


          pin = readLine("Entrez le code PIN : ")


          validatePin(index: Int)


          if (tentative == false) {

            boucle = false

          } else {

            println("Accès accordé à la Machine " + (index +1 ) + "\n")

            var question = readLine("Voulez-vous changer le code Pin (oui ou non) ? \n")
            while ((question != "oui") && (question != "non")) {

              question = readLine("Voulez-vous changer le code Pin (oui ou non) ? \n")

            }
            if (question == "oui") {

              println("Mise à jour du code PIN pour la Machine " + (index + 1))

              updatePin(index: Int)

            } else {
              println()

              var action = readLine("Voulez-vous 1) ajouter ou 2) diminuer des ingrédients ?").toInt
              while ((action != 1) & (action != 2) ) {
                println("Voulez-vous 1) ajouter ou 2) diminuer des ingrédients ?")
                action = readLine().toInt
              }

              if ( action == 1){
                var ingredients: String = readLine("Quel ingredient voulez-vous modifier ? (coffee, milk ou sugar) > ")
                while ((ingredients != "coffee") & (ingredients != "sugar") & (ingredients != "milk")) {
                  println("Quel ingredient voulez-vous modifier ? (coffee, milk ou sugar) > ")
                  ingredients = readLine()


                }

                var quantites : Int = readLine("Quelle quantité voulez-vous ajouter ? > ").toInt


                machines(index).addIngredient( ingredients , quantites)

                restockMachine(index: Int)
                println("Niveaux de stock mis à jour.")
                println("Retour au menu principal... \n")
              }else {

                var ingredients: String = readLine("Quel ingredient voulez-vous modifier ? (coffee, milk ou sugar) > ")
                while ((ingredients != "coffee") & (ingredients != "sugar") & (ingredients != "milk")) {
                  println("Quel ingredient voulez-vous modifier ? (coffee, milk ou sugar) > ")
                  ingredients = readLine()


                }

                var quantites : Int = readLine("Quelle quantité voulez-vous diminuer ? > ").toInt


                machines(index).removeIngredient( ingredients , quantites)

                restockMachine(index: Int)
                println("Niveaux de stock mis à jour.")
                println("Retour au menu principal... \n")
              }




            }
          }
        } else {

          println("Sauvegarde de " + (index + 1) + " machines dans machines.csv...")
          savecsv("machines.csv", machines)
          println("Fichier sauvegardé avec succès.")

          boucle = false
        }

      }

    }


    def serveClient(index: Int): Unit = {

      println("Veuillez s'électionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      println("> ")
      boisson = readLine().toInt

      while ((boisson != 1) && (boisson != 2) && (boisson != 3)) {
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

        while ((boisson != 4) && (boisson != 5) && (boisson != 6)) {
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


        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("(Disponible uniquement pour Cappuccino et Latte)")
        println("1) Oui")
        println("2) Non")
        println("> ")
        ajoutlait = readLine().toInt

        while ((ajoutlait != 1) && (ajoutlait != 2)) {
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

        if ((-Quantitesucre > machines(index).sugar)) {
          println("Boisson s'électionnée : " + choixboisson)
          println("Niveau de sucre : " + niveausucre + "\n")
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre machine. \n")

          Quantitesucre = 0
          Quantitelait = 0
          Quantitecafe = 0

        } else if ((-Quantitelait > machines(index).milk)) {
          println("Boisson s'électionnée : " + choixboisson)
          println("Niveau de sucre : " + niveausucre)
          println("Lait supplémentaire: " + laitsup + "\n")
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre machine. \n")

          Quantitelait = 0
          Quantitecafe = 0
          Quantitesucre = 0

        } else if ((-Quantitecafe > machines(index).coffee)) {

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

            machines(index).coffee = machines(index).coffee - 6

            if (ajoutsucre == 1) {
              machines(index).sugar = machines(index).sugar - 0
            } else if (ajoutsucre == 2) {
              machines(index).sugar = machines(index).sugar - 5
            } else if (ajoutsucre == 3) {
              machines(index).sugar = machines(index).sugar - 10
            } else {
              machines(index).sugar = machines(index).sugar - 15
            }

            if (ajoutlait == 1) {
              machines(index).milk = machines(index).milk - 170
            } else if (ajoutlait == 2) {
              machines(index).milk = machines(index).milk - 220
            } else if (ajoutlait == 3) {
              machines(index).milk = machines(index).milk - 270
            } else {
              machines(index).milk = machines(index).milk - 120
            }

          } else if (boisson == 5) {
            //moyen//

            machines(index).coffee = machines(index).coffee - 8

            if (ajoutsucre == 1) {
              machines(index).sugar = machines(index).sugar - 0
            } else if (ajoutsucre == 2) {
              machines(index).sugar = machines(index).sugar - 5
            } else if (ajoutsucre == 3) {
              machines(index).sugar = machines(index).sugar - 10
            } else {
              machines(index).sugar = machines(index).sugar - 15
            }

            if (ajoutlait == 1) {
              machines(index).milk = machines(index).milk - 200
            } else if (ajoutlait == 2) {
              machines(index).milk = machines(index).milk - 250
            } else if (ajoutlait == 3) {
              machines(index).milk = machines(index).milk - 300
            } else {
              machines(index).milk = machines(index).milk - 150
            }

          } else {
            //grand//

            machines(index).coffee = machines(index).coffee - 12

            if (ajoutsucre == 1) { //sans sucre//
              machines(index).sugar = machines(index).sugar - 0
            } else if (ajoutsucre == 2) {
              machines(index).sugar = machines(index).sugar - 5
            } else if (ajoutsucre == 3) {
              machines(index).sugar = machines(index).sugar - 10
            } else {
              machines(index).sugar = machines(index).sugar - 15
            }

            if (ajoutlait == 1) {
              machines(index).milk = machines(index).milk - 250
            } else if (ajoutlait == 2) {
              machines(index).milk = machines(index).milk - 300
            } else if (ajoutlait == 3) {
              machines(index).milk = machines(index).milk - 350
            } else {
              machines(index).milk = machines(index).milk - 200
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

          while ((ajoutlait != 1) && (ajoutlait != 2)) {
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
            Quantitesucre = (Quantitesucre - 0)
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


        if (-Quantitesucre > machines(index).sugar) {
          println("Boisson s'électionnée : " + choixboisson)
          println("Niveau de sucre : " + niveausucre + "\n")
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre machine. \n")

          Quantitesucre = 0
          Quantitelait = 0
          Quantitecafe = 0


        } else if (-Quantitelait > machines(index).milk) {
          println("Boisson s'électionnée : " + choixboisson)
          println("Niveau de sucre : " + niveausucre)
          println("Lait supplémentaire: " + laitsup + "\n")
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre machine. \n")

          Quantitesucre = 0
          Quantitelait = 0
          Quantitecafe = 0

        } else if (-Quantitecafe > machines(index).coffee) {

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

            machines(index).coffee = machines(index).coffee - 8

            if (ajoutsucre == 1) {
              machines(index).sugar = machines(index).sugar - 0
            } else if (ajoutsucre == 2) {
              machines(index).sugar = machines(index).sugar - 5
            } else if (ajoutsucre == 3) {
              machines(index).sugar = machines(index).sugar - 10
            } else {
              machines(index).sugar = machines(index).sugar - 15
            }

            if (ajoutlait == 1) {
              machines(index).milk = machines(index).milk - 50
            } else if (ajoutlait == 2) {
              machines(index).milk = machines(index).milk - 100
            } else if (ajoutlait == 3) {
              machines(index).milk = machines(index).milk - 150
            } else {
              machines(index).milk = machines(index).milk - 0
            }


          } else {

            machines(index).coffee = machines(index).coffee - 6

            if (ajoutsucre == 1) {
              machines(index).sugar = machines(index).sugar - 0
            } else if (ajoutsucre == 2) {
              machines(index).sugar = machines(index).sugar - 5
            } else if (ajoutsucre == 3) {
              machines(index).sugar = machines(index).sugar - 10
            } else {
              machines(index).sugar = machines(index).sugar - 15
            }

            if (ajoutlait == 1) {
              machines(index).milk = machines(index).milk - 150
            } else if (ajoutlait == 2) {
              machines(index).milk = machines(index).milk - 200
            } else if (ajoutlait == 3) {
              machines(index).milk = machines(index).milk - 250
            } else {
              machines(index).milk = machines(index).milk - 100
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

    }


    def restockMachine(index: Int): Unit = {


      println()
      println("Poudre de café : " + machines(index ).coffee + " g")
      println("Lait           : " + machines(index).milk + " L")
      //printf("Lait           : %.3f L \n", machines(index).milk)
      println("Sucre          : " + machines(index).sugar + " g \n")





    }


    def validatePin(index: Int): Boolean = {

      if (pin != machines(index).pincode) {
        println("Code PIN incorrect. 2 tentatives restantes.")
        pin = readLine("Entrez le code PIN : ****** ")
        if (pin != machines(index).pincode) {
          println("Code PIN incorrect. 1 tentatives restantes.")
          pin = readLine("Entrez le code PIN : ****** ")
          if (pin != machines(index).pincode) {
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


    def updatePin(index: Int): Unit = {


      var newcode = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
      while (newcode.length() != 6) {

        newcode = readLine("Entrez un nouveau code PIN à 6 chiffres > \n")

      }

      machines(index).pincode = newcode
      pin = newcode

      println("Le code PIN a été mis à jour avec succès.")
      println("Retour au menu principal...\n")


    }
  }
