import scala.io.StdIn.{readInt, readLine}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import java.io.{File, PrintWriter}
import scala.io.Source

class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    val ingredientminuscul = ingredient.toLowerCase
    if (ingredientminuscul == "milk") {
      milk += amount
    } else if (ingredientminuscul == "sugar") {
      sugar += amount
    } else if (ingredientminuscul == "coffee") {
      coffee += amount
    } else {
      println("Cet ingrédient n'existe pas.")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    val ingredientminuscul = ingredient.toLowerCase
    if (ingredientminuscul == "milk") {
      if (milk >= amount) {
        milk -= amount
        true
      } else false
    } else if (ingredientminuscul == "sugar") {
      if (sugar >= amount) {
        sugar -= amount
        true
      } else false
    } else if (ingredientminuscul == "coffee") {
      if (coffee >= amount) {
        coffee -= amount
        true
      } else false
    } else {
      println("Cet ingrédient n'existe pas.")
      false
    }
  }
}

object Main {
  var machines = ArrayBuffer[Machine]()

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    var source = Source.fromFile(filename)
    var ligne = source.getLines().toList

    source.close()

    var chargemachines = new ArrayBuffer[Machine]()

    if (ligne.length == 0) {
      println("Le fichier CSV est vide")
      return chargemachines
    }

    var entete = ligne(0)
    if (entete != "PINCODE,MILK,SUGAR,COFFEE") {
      println("le format du fichier CSV est invalide.")
      return chargemachines
    }

    var index = 1
    while (index < ligne.length) {
      var champ = ligne(index).split(",")

      if (champ.length == 4) {
        var milk = champ(1).toInt
        var sugar = champ(2).toInt
        var coffee = champ(3).toInt

        var machine = new Machine(index, champ(0), milk, sugar, coffee)
        chargemachines += machine
      }
      index = index + 1
    }

    println(chargemachines.length + " machine(s) chargée(s) avec succès.")
    return chargemachines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val fichier = new PrintWriter(new File(filename))
    fichier.println("PINCODE,MILK,SUGAR,COFFEE")
    var i = 0
    while (i < machines.length) {
      val machine = machines(i)
      fichier.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
      i += 1
    }
    fichier.close()
    println("Fichier sauvegardé avec succès.")
  }

  def validatePin(machine: Machine): Boolean = {
    var tentative = 3
    while (tentative > 0) {
      println("Entrez le code PIN pour la machine " + machine.id + ".")
      print("> ")
      val codepinentre = readLine()
      if (codepinentre == machine.pincode) return true
      tentative -= 1
      println("Code PIN incorrect. " + tentative + " tentatives restantes.")
    }
    println("Trop de tentatives échouées.")
    false
  }

  def updatePin(machine: Machine): Unit = {
    println("Mise à jour du code PIN pour la machine " + machine.id + ".")
    var nouveaupin = ""
    do {
      println("Entrez un nouveau code PIN à 6 chiffres :")
      nouveaupin = readLine()
    } while (nouveaupin.length != 6 || !nouveaupin.forall(_.isDigit))
    machine.pincode = nouveaupin
    println("Le code PIN a été mis à jour avec succès.")
  }

  def restockMachine(machine: Machine): Unit = {
    println("Le stock de café est de: " + machine.coffee + "g.")
    printf("Le stock de lait est de %.3f litres.\n", machine.milk / 1000.0)
    println("Le stock de sucre est de: " + machine.sugar + "g.")

    var ajoutcafe = -1
    while (ajoutcafe <= 0) {
      println("Ajoutez la quantité de café (grammes) :")
      ajoutcafe = readInt()
      if (ajoutcafe <= 0) println("Pas possible d'ajouter une quantité négative.")
    }
    machine.addIngredient("coffee", ajoutcafe)

    var ajoutsucre = -1
    while (ajoutsucre <= 0) {
      println("Ajoutez la quantité de sucre (grammes) :")
      ajoutsucre = readInt()
      if (ajoutsucre <= 0) println("Pas possible d'ajouter une quantité négative.")
    }
    machine.addIngredient("sugar", ajoutsucre)

    var ajoutlait = -1
    while (ajoutlait <= 0) {
      println("Ajoutez la quantité de lait (millilitres) :")
      ajoutlait = readInt()
      if (ajoutlait <= 0) println("Pas possible d'ajouter une quantité négative.")
    }
    machine.addIngredient("milk", ajoutlait)

    println("Le nouveau stock de café est de: " + machine.coffee + "g.")
    printf("Le nouveau stock de lait est de %.3f litres.\n", machine.milk / 1000.0)
    println("Le nouveau stock de sucre est de: " + machine.sugar + "g.")
  }

  def serveClient(machine: Machine): Unit = {
    val prixespresso = 2.00
    val prixcappuccino = 2.50
    val prixlattepetit = 2.70
    val prixlattemoyen = 3.20
    val prixlattegrand = 3.70

    var total = 0.0
    println("Bienvenue au distributeur de la machine " + {machine.id} + "  de Nospresso Café.")

    var typecafe = 0
    while (typecafe < 1 || typecafe > 3) {
      println("1) Expresso - CHF" + prixespresso)
      println("2) Cappuccino - CHF" + prixcappuccino)
      println("3) Latte - Petit CHF " + prixlattepetit + ", Moyen CHF " + prixlattemoyen + ", Grand CHF " + prixlattegrand)
      print("> ")
      typecafe = readInt()

      if (typecafe < 1 || typecafe > 3) {
        println("Choix incorrect. Veuillez entrer un chiffre entre 1 et 3.")
      } else {
        var stock = false

        if (typecafe == 1) {
          if (machine.removeIngredient("coffee", 8)) {
            total += prixespresso
            stock = true
          }
        } else if (typecafe == 2) {
          if (machine.removeIngredient("coffee", 6) && machine.removeIngredient("milk", 100)) {
            total += prixcappuccino
            stock = true
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

            if (taillelatte < 1 || taillelatte > 3) {
              println("Choix incorrect. Veuillez entrer un chiffre entre 1 et 3.")
            } else {
              if (taillelatte == 1) {
                if (machine.removeIngredient("coffee", 6) && machine.removeIngredient("milk", 120)) {
                  total += prixlattepetit
                  stock = true
                }
              } else if (taillelatte == 2) {
                if (machine.removeIngredient("coffee", 8) && machine.removeIngredient("milk", 150)) {
                  total += prixlattemoyen
                  stock = true
                }
              } else if (taillelatte == 3) {
                if (machine.removeIngredient("coffee", 12) && machine.removeIngredient("milk", 200)) {
                  total += prixlattegrand
                  stock = true
                }
              }
            }
          }
        }

        if (!stock) {
          println("Le stock est insuffisant, veuillez contacter un admin.")
          println("Retour au menu principale")
          return
        }
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

      if (choixsucre < 1 || choixsucre > 4) {
        println("Choix incorrect. Veuillez entrer un chiffre entre 1 et 4.")
      } else {
        if (choixsucre == 2) {
          if (machine.removeIngredient("sugar", 5)) {
            total += 0.10
          } else {
            println("Le stock de sucre est insuffisant, veuillez contacter un admin.")
            println("Retour au menu principal")
            return
          }
        } else if (choixsucre == 3) {
          if (machine.removeIngredient("sugar", 10)) {
            total += 0.20
          } else {
            println("Le stock de sucre est insuffisant, veuillez contacter un admin.")
            println("Retour au menu principal")
            return
          }
        } else if (choixsucre == 4) {
          if (machine.removeIngredient("sugar", 15)) {
            total += 0.30
          } else {
            println("Le stock de sucre est insuffisant, veuillez contacter un admin.")
            println("Retour au menu principal")
            return
          }
        }
      }
    }

    if (typecafe == 2 || typecafe == 3) {
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
              if (machine.milk >= 50 * dosedelait) {
                total += dosedelait * 0.05
                machine.milk -= 50 * dosedelait
              } else {
                println("Il n'y a plus assez de lait.")
                println("Retour au menu principal...")
                return
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
  }

  def main(args: Array[String]): Unit = {
    machines = loadcsv("machines.csv")

    while (true) {
      println("Bienvenue au distributeur Nospresso Café")
      println("Sélectionnez une machine 1-" + machines.length + ":")
      print("> ")

      val machineId = readInt() - 1
      if (machineId >= 0 && machineId < machines.length) {
        println("Les modes disponibles sont les suivants :")
        println("1 : Client")
        println("2 : Admin")
        println("3 : Quitter")
        print("> ")

        val mode = readInt()
        if (mode == 1) {
          serveClient(machines(machineId))
        } else if (mode == 2) {
          if (!validatePin(machines(machineId))) {
            return
          } else {
            println("Code correct. Accès aux informations de la marchandise.")
            var choismodeadmin = 0

            while (choismodeadmin < 1 || choismodeadmin > 2) {
              println("Que voulez-vous faire :")
              println("1) Réapprovisionner ")
              println("2) Mettre à jour le code PIN")

              choismodeadmin = readInt()
              if (choismodeadmin < 1 || choismodeadmin > 2) {
                println("Le mode choisi n'existe pas.(1 ou 2) ")
              }
            }

            if (choismodeadmin == 1) {
              restockMachine(machines(machineId))
            } else {
              updatePin(machines(machineId))
            }
          }
        } else if (mode == 3) {
          println("Merci d'avoir utilisé une machine Nospresso Café. Au revoir et à bientôt")
          savecsv("machines.csv", machines)
          return
        } else {
          println("Option invalide. Choissisez entre 1 et 3")
        }
      } else {
        println("Choix de la machine invalide. Retour au menu principal.")
      }
    }
  }
}