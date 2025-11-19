import scala.io.StdIn.{readInt, readLine}
import scala.util.Random

object Main {
  val nbMachines = 5
  val coffeeStocks = Array.fill(nbMachines)(50)
  val sugarStocks = Array.fill(nbMachines)(30)
  val milkStocks = Array.fill(nbMachines)(500)
  val machinePins = Array.fill(nbMachines)("434343") // code admin de base

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println("Entrez le code PIN pour la machine " + (machineId + 1) + ".")
      val codepinentre = readLine()
      if (codepinentre == machinePins(machineId)) {
        return true
      } else {
        attempts -= 1
        println("Code PIN incorrect. " + attempts + " tentatives restantes.")
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la machine " + (machineId + 1) + ".")
    var nouveaupin = ""
    do {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      nouveaupin = readLine()
    } while (nouveaupin.length != 6 || !nouveaupin.forall(_.isDigit))
    machinePins(machineId) = nouveaupin
    println("Le code PIN a été mis à jour avec succès.")
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Le stock de café est de " + coffeeStocks(machineId) + " grammes.")
    printf("Le stock de lait est de %.3f litres.\n", milkStocks(machineId) / 1000.0)
    println("Le stock de sucre est de " + sugarStocks(machineId) + " grammes.")
    println("Réapprovisionnement de la machine " + (machineId + 1) + ".")

    var ajoutcafe = -1
    while (ajoutcafe < 0) {
      println("Ajoutez la quantité de café (grammes) : ")
      print("> ")
      ajoutcafe = readInt()
      if (ajoutcafe < 0) {
        println("Pas possible d'ajouter une quantité négative.")
      }
    }
    var ajoutsucre = -1
    while (ajoutsucre < 0) {
      println("Ajoutez la quantité de sucre (grammes) : ")
      print("> ")
      ajoutsucre = readInt()
      if (ajoutsucre < 0) {
        println("Pas possible d'ajouter une quantité négative.")
      }
    }

    var ajoutlait = -1
    while (ajoutlait < 0) {
      println("Ajoutez la quantité de lait (millilitres) : ")
      print("> ")
      ajoutlait = readInt()
      if (ajoutlait < 0) {
        println("Pas possible d'ajouter une quantité négative.")
      }
    }

    coffeeStocks(machineId) += ajoutcafe
    milkStocks(machineId) += ajoutlait
    sugarStocks(machineId) += ajoutsucre

    println("Le nouveau stock de café est de " + coffeeStocks(machineId) + " grammes.")
    printf("Le  nouveau stock de lait est de %.3f litres.\n", milkStocks(machineId) / 1000.0)
    println("Le nouveau stock de sucre est de " + sugarStocks(machineId) + " grammes.")
    println("Réapprovisionnement terminé.")
    println("Retour au menu principal...")
    Thread.sleep(1500)
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    val prixespresso = 2.00
    val prixcappuccino = 2.50
    val prixlattepetit = 2.70
    val prixlattemoyen = 3.20
    val prixlattegrand = 3.70

    var total = 0.0

    println("Bienvenue au distributeur de la machine " + machineId + " de Nospresso Café.")

    var typecafe = 0
    while (typecafe < 1 || typecafe > 3) {
      println("Veuillez sélectionner votre café :")
      println(s"1) Expresso - CHF" + prixespresso)
      println(s"2) Cappuccino - CHF" + prixcappuccino)
      println("3) Latte - Petit CHF " + prixlattepetit + ", Moyen CHF " + prixlattemoyen + ", Grand CHF " + prixlattegrand)
      print("> ")
      typecafe = readInt()

      if (typecafe == 1) {
        if (coffeeStocks(machineId) >= 8) {
          total += prixespresso
          coffeeStocks(machineId) -= 8
        } else {
          println("Il n'y a plus assez de café.")
          return false
        }
      } else if (typecafe == 2) {
        if (coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 100) {
          total += prixcappuccino
          coffeeStocks(machineId) -= 6
          milkStocks(machineId) -= 100
        } else {
          if (coffeeStocks(machineId) < 6) {
            println("Il n'y a plus assez de café.")
          }
          if (milkStocks(machineId) < 100) {
            println("Il n'y a plus assez de lait.")
          }
          return false
        }
      } else if (typecafe == 3) {
        var taillelatte = 0
        while (taillelatte < 1 || taillelatte > 3) {
          println("Veuillez sélectionner la taille du Latte :")
          println("1) Petit")
          println("2) Moyen")
          println("3) Grand")
          print("> ")
          taillelatte = readInt()

          if (taillelatte == 1) {
            if (coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 120) {
              total += prixlattepetit
              coffeeStocks(machineId) -= 6
              milkStocks(machineId) -= 120
            } else {
              if (coffeeStocks(machineId) < 6) {
                println("Il n'y a plus assez de café.")
              }
              if (milkStocks(machineId) < 120) {
                println("Il n'y a plus assez de lait.")
              }
              return false
            }
          } else if (taillelatte == 2) {
            if (coffeeStocks(machineId) >= 8 && milkStocks(machineId) >= 150) {
              total += prixlattemoyen
              coffeeStocks(machineId) -= 8
              milkStocks(machineId) -= 150
            } else {
              if (coffeeStocks(machineId) < 8) {
                println("Il n'y a plus assez de café.")
              }
              if (milkStocks(machineId) < 150) {
                println("Il n'y a plus assez de lait.")
              }
              return false
            }
          } else if (taillelatte == 3) {
            if (coffeeStocks(machineId) >= 12 && milkStocks(machineId) >= 200) {
              total += prixlattegrand
              coffeeStocks(machineId) -= 12
              milkStocks(machineId) -= 200
            } else {
              if (coffeeStocks(machineId) < 12) {
                println("Il n'y a plus assez de café.")
              }
              if (milkStocks(machineId) < 200) {
                println("Il n'y a plus assez de lait.")
              }
              return false
            }
          } else {
            println("Choix incorrect. Cette taille n'existe pas.")
          }
        }
      } else {
        println("Choix incorrect. Veuillez entrer un chiffre entre 1 et 3.")
      }
    }

    var choixsucre = 0
    while (choixsucre < 1 || choixsucre > 4) {
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")
      choixsucre = readInt()

      if (choixsucre == 2) {
        if (sugarStocks(machineId) >= 5) {
          total += 0.10
          sugarStocks(machineId) -= 5
        } else {
          println("Il n'y a plus assez de sucre.")
          println("Retour au menu principal...")
          return false
        }
      } else if (choixsucre == 3) {
        if (sugarStocks(machineId) >= 10) {
          total += 0.20
          sugarStocks(machineId) -= 10
        } else {
          println("Il n'y a plus assez de sucre.")
          println("Retour au menu principal...")
          return false
        }
      } else if (choixsucre == 4) {
        if (sugarStocks(machineId) >= 15) {
          total += 0.30
          sugarStocks(machineId) -= 15
        } else {
          println("Il n'y a plus assez de sucre.")
          println("Retour au menu principal...")
          return false
        }
      } else if (choixsucre != 1) {
        println("Choix incorrect. Veuillez entrer un chiffre entre 1 et 4.")
      }
    }

    if ((typecafe == 2 || typecafe == 3)) {
      var ajoutelait = 0
      while (ajoutelait != 1 && ajoutelait != 2) {
        println("Souhaitez-vous ajouter une dose de lait ? (jusqu'à 3 au maximum)")
        println("1) Oui")
        println("2) Non")
        print("> ")
        ajoutelait = readInt()

        if (ajoutelait == 1) {
          var dosedelait = 0
          while (dosedelait < 1 || dosedelait > 3) {
            println("Combien de dose de lait voulez-vous ajouter ? (1, 2 ou 3)")
            print("> ")
            dosedelait = readInt()

            if (dosedelait >= 1 && dosedelait <= 3) {
              if (milkStocks(machineId) >= 50 * dosedelait) {
                total += dosedelait * 0.05
                milkStocks(machineId) -= 50 * dosedelait
              } else {
                println("Il n'y a plus assez de lait.")
                println("Retour au menu principal...")
                return false
              }
            } else {
              println("Choix incorrect. Veuillez entrer un chiffre entre 1 et 3.")
            }
          }
        } else if (ajoutelait != 2) {
          println("Choix incorrect. Veuillez entrer 1 pour Oui ou 2 pour Non.")
        }
      }
    }

    val alphanumcara = "AZERTYUIOPQSDFGHJKLMWXCVBNazertyuiopmlkjhgfdsqwxcvbn0123456789"
    val longeur = 5
    var code = ""
    var i = 0
    while (i < longeur) {
      val codealeatoir = Random.nextInt(alphanumcara.length)
      code += alphanumcara(codealeatoir)
      i += 1
    }

    printf("Total à payer : CHF  %.2f \n", +total)
    println("Veuillez payer en utilisant Twint. ")
    println("Le code du paiement Twint est: " + code)
    println("En attente de validation du paiement...")
    Thread.sleep(2987)

    println("Merci ! Votre paiement a été accepté.")
    println("Préparation de votre boisson...")
    println("[...]")
    Thread.sleep(3000)
    if (typecafe == 1) {
      println("Votre Expresso est prêt ! Bonne dégustation !")
    } else if (typecafe == 2) {
      println("Votre Cappuccino est prêt ! Bonne dégustation !")
    } else {
      println("Votre Latte est prêt ! Bonne dégustation !")
    }
    true
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      println("Bienvenue au distributeur Nospresso Café")
      println("Veuillez sélectionner une machine (de 1 à 5) :")
      print("> ")

      var machineId = readInt()
      if (machineId < 1 || machineId > 5) {
        println("Cette machine n'existe pas. Veuillez entrer un nombre entre 1 et 5.")
      } else {
        machineId = machineId - 1

        println("Les modes disponibles sont les suivants :")
        println("1 : Client")
        println("2 : Admin")
        println("3 : Quitter")
        print("> ")
        val mode = readInt()

        if (mode == 1) {
          if (!serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)) {
            println("Transaction échouée. Veuillez contacter un Admin ou changer de machine.")
          }
        } else if (mode == 2) {
          if (!validatePin(machineId, machinePins)) {
            return
          } else {
            println("Code correct. Accès aux informations de la marchandise.")
            println("Que voulez-vous faire :")
            println("1) Réapprovisionner ")
            println("2) Mettre à jour le code PIN")

            var choismodeadmin = 0
            while (choismodeadmin < 1 || choismodeadmin > 2) {
              println("Veuillez choisir un mode (1 ou 2) :")
              print("> ")
              choismodeadmin = readInt()

              if (choismodeadmin == 1) {
                restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
              } else if (choismodeadmin == 2) {
                updatePin(machineId, machinePins)
              }
              else {
                println("Choix invalide.")
              }
            }
          }
        }
          else if (mode == 3) {
            println("Merci d'avoir utilisé une machine Nospresso Café. Au revoir et à bientôt")
            return
          } else {
            println("Choix invalide. Retour au menu principale.")
          }
        }
      }
    }
}
