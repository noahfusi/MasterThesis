import scala.io.StdIn._
import scala.util.Random
object Main {
  def main(args: Array[String]): Unit = {
    // Nombre de machines
        val nbMachines = 5

        // Tableau des niveaux de stock pour chaque machine
        var coffeeStocks = Array(50.0, 30.0, 70.0, 40.0, 60.0)  // en grammes
        var sugarStocks = Array(30.0, 20.0, 60.0, 40.0, 50.0)   // en grammes
        var milkStocks = Array(0.5, 0.6, 0.4, 0.3, 0.7)         // en litres

        // Codes PIN des machines
        var machinePins = Array("434343", "434343", "434343", "434343", "434343")

        var mode: Int = 0
        var machineEnPanne = false

        // Menu principal
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
            println("Veuillez sélectionner la machine (1-5) :")
            print("> ")
            val machineId = readInt() - 1

            if (machineId < 0 || machineId >= nbMachines) {
              println("Machine invalide.")
            } else {
              if (machineEnPanne) {
                println("La machine est en panne, veuillez contacter le technicien.")
              } else {
                // Affichage du stock initial de la machine choisie
                println(s"Stock actuel de la machine ${machineId + 1} :")
                println(s"Café : ${coffeeStocks(machineId)} g")
                println(s"Sucre : ${sugarStocks(machineId)} g")
                println(s"Lait : ${milkStocks(machineId)} L")

                println("Veuillez sélectionner votre boisson :")
                println("1) Expresso - CHF 2.00")
                println("2) Cappuccino - CHF 2.50")
                println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
                print("> ")

                val Choixdelaboisson = readInt()

                var prixFinal = 0.0
                var boisson = ""
                var cafeNecessaire = 0.0
                var laitNecessaire = 0.0
                var sucreNecessaire = 0.0

                if (Choixdelaboisson == 1) {
                  boisson = "Expresso"
                  prixFinal = 2.0
                  cafeNecessaire = 8.0
                } else if (Choixdelaboisson == 2) {
                  boisson = "Cappuccino"
                  prixFinal = 2.5
                  cafeNecessaire = 6.0
                  laitNecessaire = 0.1
                } else if (Choixdelaboisson == 3) {
                  println("Veuillez sélectionner la taille de votre Latte :")
                  println("1) Petit - CHF 2.70")
                  println("2) Moyen - CHF 3.20")
                  println("3) Grand - CHF 3.70")
                  print("> ")

                  val tailleLatte = readInt()

                  if (tailleLatte == 1) {
                    boisson = "Latte Petit"
                    prixFinal = 2.7
                    cafeNecessaire = 6.0
                    laitNecessaire = 0.12
                  } else if (tailleLatte == 2) {
                    boisson = "Latte Moyen"
                    prixFinal = 3.2
                    cafeNecessaire = 8.0
                    laitNecessaire = 0.15
                  } else if (tailleLatte == 3) {
                    boisson = "Latte Grand"
                    prixFinal = 3.7
                    cafeNecessaire = 12.0
                    laitNecessaire = 0.2
                  } else {
                    println("Entrée invalide.")
                  }
                } else {
                  println("Entrée invalide.")
                }

                if (boisson != "") {
                  println(s"Boisson sélectionnée : $boisson")
                  println(s"Prix de la boisson : CHF $prixFinal")

                  // Sélection du sucre
                  println("Souhaitez-vous ajouter du sucre ?")
                  println("1) Sans sucre")
                  println("2) Peu (5g) - CHF 0.10")
                  println("3) Moyen (10g) - CHF 0.20")
                  println("4) Beaucoup (15g) - CHF 0.30")
                  print("> ")

                  val quantitedesucre = readInt()

                  if (quantitedesucre == 1) {
                    sucreNecessaire = 0.0
                  } else if (quantitedesucre == 2) {
                    sucreNecessaire = 5.0
                    prixFinal += 0.10
                  } else if (quantitedesucre == 3) {
                    sucreNecessaire = 10.0
                    prixFinal += 0.20
                  } else if (quantitedesucre == 4) {
                    sucreNecessaire = 15.0
                    prixFinal += 0.30
                  } else {
                    println("Entrée invalide.")
                  }

                  // Vérification des stocks
                  if (coffeeStocks(machineId) >= cafeNecessaire && sugarStocks(machineId) >= sucreNecessaire && milkStocks(machineId) >= laitNecessaire) {
                    println(s"Prix total : CHF $prixFinal")
                    println("Veuillez payer en utilisant Twint.")

                    // Générer un code de paiement aléatoire
                    val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
                    var codeTwint = ""
                    for (_ <- 1 to 5) {
                      val randomIndex = scala.util.Random.nextInt(chars.length)
                      codeTwint += chars(randomIndex)
                    }
                    println(s"Votre code de paiement est : $codeTwint")

                    // Attente pour simuler le paiement
                    Thread.sleep(3000)
                    println("Paiement confirmé.")
                    println("Préparation de votre boisson...")

                    // Attente pour simuler la préparation
                    Thread.sleep(5000)
                    println("Votre boisson est prête ! Bonne dégustation !")

                    // Mise à jour des stocks
                    coffeeStocks(machineId) -= cafeNecessaire
                    sugarStocks(machineId) -= sucreNecessaire
                    milkStocks(machineId) -= laitNecessaire
                  } else {
                    println("Erreur : Quantité insuffisante pour préparer la boisson sélectionnée.")
                  }
                }
              }
            }
          } else if (mode == 2) {
            // Mode Admin
            println("Mode Admin")
            print("Entrez le code PIN : ")
            var pin = readLine()

            var accessGranted = false
            var attempts = 3
            while (attempts > 0 && !accessGranted) {
              if (machinePins.contains(pin)) {
                accessGranted = true
              } else {
                attempts -= 1
                if (attempts > 0) {
                  println(s"Code PIN incorrect. Tentatives restantes : $attempts")
                  print("Veuillez entrer le code PIN : ")
                  pin = readLine()
                }
              }
            }

            if (accessGranted) {
              println("Accès autorisé.")

              println("Que voulez-vous faire ?")
              println("1) Réapprovisionner une machine")
              println("2) Mettre à jour le code PIN")
              print("> ")
              val choixAdmin = readInt()

              if (choixAdmin == 1) {
                println("Veuillez sélectionner la machine (1-5) :")
                print("> ")
                val machineId = readInt() - 1

                if (machineId >= 0 && machineId < nbMachines) {
                  // Affichage du stock initial de la machine choisie
                  println(s"Stock actuel de la machine ${machineId + 1} :")
                  println(s"Café : ${coffeeStocks(machineId)} g")
                  println(s"Sucre : ${sugarStocks(machineId)} g")
                  println(s"Lait : ${milkStocks(machineId)} L")

                  println("Réapprovisionnement des stocks.")
                  println("Que voulez-vous réapprovisionner ?")
                  println("1) Café")
                  println("2) Sucre")
                  println("3) Lait")
                  print("> ")
                  val choixReapprovisionnement = readInt()

                  if (choixReapprovisionnement == 1) {
                    println("Veuillez entrer la quantité de café à ajouter (en grammes) : ")
                    val ajoutCafe = readDouble()
                    coffeeStocks(machineId) += ajoutCafe
                    println(s"Stocks mis à jour : Café : ${coffeeStocks(machineId)} g")
                  } else if (choixReapprovisionnement == 2) {
                    println("Veuillez entrer la quantité de sucre à ajouter (en grammes) : ")
                    val ajoutSucre = readDouble()
                    sugarStocks(machineId) += ajoutSucre
                    println(s"Stocks mis à jour : Sucre : ${sugarStocks(machineId)} g")
                  } else if (choixReapprovisionnement == 3) {
                    println("Veuillez entrer la quantité de lait à ajouter (en litres) : ")
                    val ajoutLait = readDouble()
                    milkStocks(machineId) += ajoutLait
                    println(s"Stocks mis à jour : Lait : ${milkStocks(machineId)} L")
                  } else {
                    println("Choix invalide.")
                  }
                } else {
                  println("Machine invalide.")
                }
              } else if (choixAdmin == 2) {
                println("Veuillez sélectionner la machine (1-5) :")
                print("> ")
                val machineId = readInt() - 1

                if (machineId >= 0 && machineId < nbMachines) {
                  println("Veuillez entrer le nouveau code PIN :")
                  val newPin = readLine()
                  machinePins(machineId) = newPin
                  println(s"Code PIN de la machine ${machineId + 1} mis à jour.")
                } else {
                  println("Machine invalide.")
                }
              } else {
                println("Choix invalide.")
              }
            } else {
              println("Trop de tentatives échouées. Fin du programme.")
              System.exit(0)
            }
          }
        }

        println("Merci d'avoir utilisé le distributeur Nospresso Café. À bientôt !")
      }
    }