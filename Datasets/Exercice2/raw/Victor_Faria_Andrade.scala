import scala.io.StdIn._
import scala.util.Random

object Nospresso {
  val nbMachines = 5
  val machinePins = Array("434343", "434343", "434343", "434343", "434343")
  val coffeeStocks = Array(50, 50, 50, 50, 50)
  val sugarStocks = Array(30, 30, 30, 30, 30)
  val milkStocks = Array(500, 500, 500, 500, 500)

  def pin(): String = {
    Random.alphanumeric.take(5).mkString
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var essais = 0
    var essaisPin = ""
    while (essais < 3) {
      println("Entrez le code PIN : ")
      print("> ")
      essaisPin = readLine()
      if (essaisPin == machinePins(machineId)) {
        println("Accès accordé à la Machine " + (machineId +1) + ".")
        return true
      } else {
        essais = essais + 1
        if(essais < 2){
          println("Code PIN incorrect. " + (3-essais) + " tentatives restantes.")
          print("> ")
        } else {
          println("Code PIN incorrect. " + (3-essais) + " tentative restante.")
          print("> ")
        }
      }
    }
    println("Trop de tentatives échouées. Fin du programme.")
    return false
  }
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres > ")
    var nouveauPin = readLine()
    while (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit)) {
      println("Le code PIN ne contient pas 6 chiffres > ")
      nouveauPin = readLine()
    }
    machinePins(machineId) = nouveauPin
    println("Le code PIN pour la machine " + (machineId + 1) + " a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    var choix_boisson: Int = 0
    var choix_sucre: Int = 0
    var choix_lait: Int = 0
    var choix_dose: Int = 0
    var latte_taille_choix: Int = 0

    println("Veuillez sélectionner votre boisson :")
    println("")
    println("1) Espresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")
    choix_boisson = readInt()

    if(choix_boisson == 1 || choix_boisson == 2) {
      println("Souhaitez-vous ajouter du sucre ?")
      println("")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print("> ")
      choix_sucre = readInt()
    }
    if(choix_boisson == 2) {
      println("Souhaitez-vous ajouter du lait ?")
      println("")
      println("1) Oui")
      println("2) Non")
      println("")
      print("> ")
      choix_lait = readInt()
      if(choix_lait == 1) {
        while(choix_dose < 1 || choix_dose > 3) {
          println("Combien de doses ? (maximum 3 doses, 1 dose = 50ml)")
          print("> ")
          choix_dose = readInt()
          if(choix_dose < 1 || choix_dose > 3) {
            println("Entrée non valide, veuillez réessayer.")
          }
        }
      }
    }
    if(choix_boisson == 1) {
      if(coffeeStocks(machineId) >= 8) {
        if(choix_sucre == 1) {
          println("Prix total : CHF 2.00")
        }
        if(choix_sucre == 2) {
          if(sugarStocks(machineId) >= 5) {
            sugarStocks(machineId) -= 5
            println("Prix total : CHF 2.00 + CHF 0.10 = CHF 2.10")
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            return false
          }
        }
        if(choix_sucre == 3) {
          if(sugarStocks(machineId) >= 10) {
            sugarStocks(machineId) -= 10
            println("Prix total : CHF 2.00 + CHF 0.20 = CHF 2.20")
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            return false
          }
        }
        if(choix_sucre == 4) {
          if(sugarStocks(machineId) >= 15) {
            sugarStocks(machineId)-= 15
            println("Prix total : CHF 2.00 + CHF 0.30 = CHF 2.30")
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            return false
          }
        }
        coffeeStocks(machineId) -= 8
      } else {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false
      }
    }
    if(choix_boisson == 2) {
      if(coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 100) {
        if(choix_sucre == 1) {
          if(choix_lait == 1) {
            println("Prix total : CHF 2.50 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.50 + (choix_dose * 0.05)))
            milkStocks(machineId) -= choix_dose * 50
          } else {
            println("Prix total : CHF 2.50")
            milkStocks(machineId) -= choix_dose * 50
          }
        }
        if(choix_sucre == 2) {
          if(sugarStocks(machineId) >= 5) {
            if(choix_lait == 1) {
              println("Prix total : CHF 2.00 + CHF 0.10 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.50 + 0.10 + (choix_dose * 0.05)))
              sugarStocks(machineId) -= 5
              milkStocks(machineId) -= choix_dose * 50
            } else {
              println("Prix total : CHF 2.50 + CHF 0.10 = CHF 2.60")
              sugarStocks(machineId) -= 5
              milkStocks(machineId) -= choix_dose * 50
            }
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            return false
          }
        }
        if(choix_sucre == 3) {
          if(sugarStocks(machineId) >= 10) {
            if(choix_lait == 1) {
              println("Prix total : CHF 2.50 + CHF 0.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.50 + 0.20 + (choix_dose * 0.05)))
              sugarStocks(machineId) -= 10
              milkStocks(machineId) -= choix_dose * 50
            } else {
              println("Prix total : CHF 2.50 + CHF 0.20 = CHF 2.70")
              sugarStocks(machineId) -= 10
              milkStocks(machineId) -= choix_dose * 50
            }
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            return false
          }
        }
        if(choix_sucre == 4) {
          if(sugarStocks(machineId) >= 15) {
            if(choix_lait == 1) {
              println("Prix total : CHF 2.50 + CHF 0.30 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.50 + 0.30 + (choix_dose * 0.05)))
              sugarStocks(machineId) -= 15
              milkStocks(machineId) -= choix_dose * 50
            }else {
              println("prix total : CHF 2.50 + CHF 0.30 = CHF 2.80")
              sugarStocks(machineId) -= 15
              milkStocks(machineId) -= choix_dose * 50
            }
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            return false
          }
        }
        coffeeStocks(machineId) -= 6
        milkStocks(machineId) -= 100
      } else {
        if(coffeeStocks(machineId) < 6) {
          println("Erreur : Quantité de poudre de café pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if(milkStocks(machineId) < 100) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        return false
      }
    }
    if(choix_boisson == 3) {
      if(coffeeStocks(machineId) >= 6 && milkStocks(machineId) >= 120) {
        println("Veuillez choisir la taille de votre Latte :")
        println("")
        println("1) Petit - CHF 2.70")
        println("2) Moyen - CHF 3.20")
        println("3) Grand - CHF 3.70")
        println("")
        print("> ")
        latte_taille_choix = readInt()

        println("Souhaitez-vous ajouter du sucre ?")
        println("")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        println("")
        print("> ")
        choix_sucre = readInt()
        println("")
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("")
        println("1) Oui")
        println("2) Non")
        println("")
        print("> ")
        choix_lait = readInt()
        if(choix_lait == 1) {
          while(choix_dose < 1 || choix_dose > 3) {
            println("Combien de doses ? (maximum 3 doses, 1 dose = 50ml)")
            print("> ")
            choix_dose = readInt()
            if(choix_dose < 1 || choix_dose > 3) {
              println("Entrée non valide, veuillez réessayer.")
            }
          }
        }
        if(latte_taille_choix == 1) {
          if(choix_sucre == 1) {
            if(choix_lait == 1) {
              println("Prix total : CHF 2.70 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.70 + (choix_dose * 0.05)))
              milkStocks(machineId) -= choix_dose * 50
            } else {
              println("Prix total : CHF 2.70")
            }
          }
          if(choix_sucre == 2) {
            if(sugarStocks(machineId) >= 5) {
              sugarStocks(machineId) -= 5
              if(choix_lait == 1) {
                println("Prix total : CHF 2.70 + CHF 0.10 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.70 + 0.10 + (choix_dose * 0.05)))
                milkStocks(machineId) -= choix_dose * 50
              } else {
                println("Prix total : CHF 2.70 + CHF 0.10 = CHF 2.80")
              }
            } else {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              return false
            }
          }
          if(choix_sucre == 3) {
            if(sugarStocks(machineId) >= 10) {
              sugarStocks(machineId) -= 10
              if(choix_lait == 1) {
                println("Prix total : CHF 2.70 + CHF 0.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.70 + 0.20 + (choix_dose * 0.05)))
                milkStocks(machineId) -= choix_dose * 50
              } else {
                println("Prix total : CHF 2.70 + CHF 0.20 = CHF 2.90")
              }
            } else {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              return false
            }
          }
          if(choix_sucre == 4) {
            if(sugarStocks(machineId) >= 15) {
              sugarStocks(machineId) -= 15
              if(choix_lait == 1) {
                println("Prix total : CHF 2.70 + CHF 0.30 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.70 + 0.30 + (choix_dose * 0.05)))
                milkStocks(machineId) -= choix_dose * 50
              } else {
                println("Prix total : CHF 2.70 + CHF 0.30 = CHF 3.00")
              }
            } else {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              return false
            }
          }
          coffeeStocks(machineId) -= 6
          milkStocks(machineId) -= 120
        }
        if(latte_taille_choix == 2) {
          if(coffeeStocks(machineId) >= 8 && milkStocks(machineId) >= 150) {
            if(choix_sucre == 1) {
              if(choix_lait ==1) {
                println("Prix total : CHF 3.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + (choix_dose * 0.05)))
              } else {
                println("Prix total : CHF 3.20")
              }
            }
            if(choix_sucre == 2) {
              if(sugarStocks(machineId) >= 5) {
                sugarStocks(machineId) -= 5
                if(choix_lait == 1) {
                  println("Prix total : CHF 3.20 + CHF 0.10 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + 0.10 + (choix_dose * 0.05)))
                  milkStocks(machineId) -= choix_dose * 50
                } else {
                  println("Prix total : CHF 3.20 + CHF 0.10 = CHF 3.30")
                }
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
            }
            if(choix_sucre == 3) {
              if(sugarStocks(machineId) >= 10) {
                sugarStocks(machineId) -= 10
                if(choix_lait == 1) {
                  println("Prix total : CHF 3.20 + CHF 0.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + 0.20 + (choix_dose * 0.05)))
                  milkStocks(machineId) -= choix_dose * 50
                } else {
                  println("Prix total : CHF 3.20 + CHF 0.20 = CHF 3.40")
                }
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                return false
              }
            }
            if(choix_sucre == 4) {
              if(sugarStocks(machineId) >= 15) {
                sugarStocks(machineId) -= 15
                if(choix_lait == 1) {
                  println("Prix total : CHF 3.20 + CHF 0.30 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + 0.30 + (choix_dose * 0.05)))
                  milkStocks(machineId) -= choix_dose * 50
                } else {
                  println("Prix total : CHF 3.20 + CHF 0.30 = CHF 3.50")
                }
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                return false
              }
            }
          }
          coffeeStocks(machineId) -= 8
          milkStocks(machineId) -= 150
        }
        if(latte_taille_choix == 3) {
          if(coffeeStocks(machineId) >= 12 && milkStocks(machineId) >= 200) {
            if(choix_sucre == 1) {
              if(choix_lait ==1) {
                println("Prix total : CHF 3.70 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.70 + (choix_dose * 0.05)).toDouble)
              } else {
                println("Prix total : CHF 3.70")
              }
            }
            if(choix_sucre == 2) {
              if(sugarStocks(machineId) >= 5) {
                sugarStocks(machineId) -= 5
                if(choix_lait == 1) {
                  println("Prix total : CHF 3.70 + CHF 0.10 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.70 + 0.10 + (choix_dose * 0.05)))
                  milkStocks(machineId) -= choix_dose * 50
                } else {
                  println("Prix total : CHF 3.70 + CHF 0.10 = CHF 3.80")
                }
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                return false
              }
            }
            if(choix_sucre == 3) {
              if(sugarStocks(machineId) >= 10) {
                sugarStocks(machineId) -= 10
                if(choix_lait == 1) {
                  println("Prix total : CHF 3.70 + CHF 0.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.70 + 0.20 + (choix_dose * 0.05)))
                  milkStocks(machineId) -= choix_dose * 50
                } else {
                  println("Prix total : CHF 3.70 + CHF 0.20 = CHF 3.90")
                }
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                return false
              }
            }
            if(choix_sucre == 4) {
              if(sugarStocks(machineId) >= 15) {
                sugarStocks(machineId) -= 15
                if(choix_lait == 1) {
                  println("Prix total : CHF 3.70 + CHF 0.30 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.70 + 0.30 + (choix_dose * 0.05)))
                  milkStocks(machineId) -= choix_dose * 50
                } else {
                  println("Prix total : CHF 3.70 + CHF 0.30 = CHF 4.00")
                }
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                return false
              }
            }
          }
          coffeeStocks(machineId) -= 12
          milkStocks(machineId) -= 200
        }
      } else {
        if(coffeeStocks(machineId) < 6) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if(milkStocks(machineId) < 120) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        return false
      }
    }
    println("")
    println("Veuillez payer en utilisant Twint.")
    println("Votre code de paiement est : " + pin())
    println("(En attente de validation du paiement...)")
    Thread.sleep(5000)
    println("")
    println("Merci ! Votre paiement a été accepté.")
    Thread.sleep(1000)
    println("")
    println("Préparation de votre boisson...")
    Thread.sleep(1000)
    println("[...]")
    Thread.sleep(1000)
    if(choix_boisson ==1) {
      println("Votre Espresso est prêt ! Bonne dégustation !")
      println("")
    } else if(choix_boisson == 2) {
      println("Votre Cappuccino est prêt ! Bonne dégustation !")
      println("")
    } else if(choix_boisson == 3) {
      println("Votre Latte est prêt ! Bonne dégustation !")
      println("")
    }
    return true
  }
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    Thread.sleep(1000)
    println("")
    println("Niveaux de stock actuels:")
    var lait_stock = milkStocks(machineId).toDouble/1000
    println("")
    println("   Poudre de café : " + coffeeStocks(machineId) + "g")
    println("   Sucre          : " + sugarStocks(machineId) + "g")
    println("   Lait           : " + lait_stock + "L")
    println("")
    println("Réapprovisionnement des stocks...")
    print("Entrez les quantités à ajouter : ")
    println("")
    print("   Poudre de café > ")
    var c = readInt()
    print("   Sucre          > ")
    var s = readInt()
    print("   Lait           > ")
    var l = readDouble()
    println("Ajout de : " + c + "g de poudre de café, " + s + "g de sucre, et " + l + "L de lait dans les stocks.")
    println("")
    Thread.sleep(1000)
    println("Niveaux de stocks mis à jour pour la machine " + (machineId +1) + " :")
    println("")
    println("   Poudre de café : " + (coffeeStocks(machineId) + c) + "g")
    println("   Sucre          : " + (sugarStocks(machineId) + s) + "g")
    println("   Lait           : " + (lait_stock + l) + "L")
    Thread.sleep(1000)
    println("")
    println("Les stocks ont été mis à avec succès.")
    println("Retour au menu principal...")
    Thread.sleep(2000)
  }
  def main(args: Array[String]): Unit = {
    var stayinloop = true
    var action: Int = 0

    while (stayinloop) {
      println("Sélectionnez une machine (1-5) :")
      print("> ")
      var machineId = readInt() - 1

      while (machineId < 0|| machineId >= nbMachines){
        println("Choix invalide.")
        print("Machine sélectionnée (1-5) ")
        print("> ")
        machineId = readInt() - 1
      }

      println("\n         Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      print("> ")
      action = readInt()
      while (action !=1 && action !=2 && action != 3){
        println("\n         Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print("> ")
        action = readInt()
      }
      if (action == 1) {
        if (machineId >= 0 && machineId < nbMachines) {
          if (serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)) {
            println("Boisson préparée avec succès !")
          } else {
            println("Erreur dans la préparation de la boisson, veuillez vérifier les stocks.")
          }
        }
      }
      else if (action == 2) {
        println("Mode Admin")
        if (machineId >= 0 && machineId < nbMachines) {
          if (validatePin(machineId, machinePins)) {
            println("Accès Administrateur autorisé.")
            println("Que voulez-vous faire ?")
            println("1) Mettre à jour le code PIN")
            println("2) Réapprovisionner les stocks")
            print("> ")
            var admin_choix = readInt()
            while (admin_choix != 1 && admin_choix != 2){
              println("Choix invalide.")
              println("Que voulez-vous faire ?")
              println("1) Mettre à jour le code PIN")
              println("2) Réapprovisionner les stocks")
              print("> ")
              admin_choix = readInt()
            }
            if (admin_choix == 1) {
              updatePin(machineId, machinePins)
            } else  {
              restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
            }
          }
        }
      } else {
        stayinloop = false
        println("Au revoir !")
        println("")
      }
    }
  }
}