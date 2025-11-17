import scala.io.StdIn._
import scala.util.Random

object Nospresso {

  def pin(): String = {
    Random.alphanumeric.take(5).mkString
  }

  def main(args: Array[String]): Unit = {

    var action : Short = 0
    var AdminPIN : String = "434343"
    var EntreePIN : String = "0"

    var stayinloop : Boolean = true

    var cafeStock : Long = 50
    var sucreStock : Long = 30
    var laitStock : Double = 0.5

    var choix_boisson : Short = 0
    var choix_sucre : Short = 0
    var choix_lait : Short = 0
    var choix_dose : Short = 0
    var latte_taille_choix : Short = 0

    var boisson_preparation : Boolean = false

    val word : String = pin()

    while(stayinloop == true) {
      while(action < 1 || action > 3) {

        choix_dose = 0

        println("")
        println("         Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print("> ")
        action = readShort()

        if(action < 1 || action > 3) {
          println("Entrée non valide, veuillez réessayer.")
          println("")
        }
      }

      if(action == 1) {
        println("Veuillez sélectionner votre boisson :")
        println("")
        println("1) Espresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print("> ")
        choix_boisson = readShort()

        if(choix_boisson == 1 || choix_boisson == 2) {
          println("Souhaitez-vous ajouter du sucre ?")
          println("")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          choix_sucre = readShort()
        }
        if(choix_boisson == 2) {
          println("Souhaitez-vous ajouter du lait ?")
          println("")
          println("1) Oui")
          println("2) Non")
          println("")
          print("> ")
          choix_lait = readShort()
          if(choix_lait == 1) {
            while(choix_dose < 1 || choix_dose > 3) {
              println("Combien de doses ? (maximum 3 doses, 1 dose = 50ml)")
              print("> ")
              choix_dose = readShort()
              if(choix_dose < 1 || choix_dose > 3) {
                println("Entrée non valide, veuillez réessayer.")
              }
            }
          }
        }
        if(choix_boisson == 1) {
          if(cafeStock >= 8) {
            if(choix_sucre == 1) {
              println("Prix total : CHF 2.00")
              boisson_preparation = true
            }
            if(choix_sucre == 2) {
              if(sucreStock >= 5) {
                sucreStock -= 5
                println("Prix total : CHF 2.00 + CHF 0.10 = CHF 2.10")
                boisson_preparation = true
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                action = 0
              }
            }
            if(choix_sucre == 3) {
              if(sucreStock >= 10) {
                sucreStock -= 10
                println("Prix total : CHF 2.00 + CHF 0.20 = CHF 2.20")
                boisson_preparation = true
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                action = 0
              }
            }
            if(choix_sucre == 4) {
              if(sucreStock >= 15) {
                sucreStock -= 15
                println("Prix total : CHF 2.00 + CHf 0.30 = CHF 2.30")
                boisson_preparation = true
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                action = 0
              }
            }
            cafeStock -= 8
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            action = 0
          }
        }
        if(choix_boisson == 2) {
          if(cafeStock >= 6 || laitStock >= 0.1) {
            if(choix_sucre == 1) {
              if(choix_lait == 1) {
                println("Prix total : CHF 2.50 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.50 + (choix_dose * 0.05)))
                boisson_preparation = true
                laitStock -= choix_dose * 0.05
              } else {
                println("Prix total : CHF 2.50")
                boisson_preparation = true
                laitStock -= choix_dose * 0.05
              }
            }
            if(choix_sucre == 2) {
              if(sucreStock >= 5) {
                if(choix_lait == 1) {
                  println("Prix total : CHF 2.00 + CHF 0.10 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.50 + 0.10 + (choix_dose * 0.05)))
                  boisson_preparation = true
                  sucreStock -= 5
                  laitStock -= choix_dose * 0.05
                } else {
                  println("Prix total : CHF 2.50 + CHF 0.10 = CHF 2.60")
                  boisson_preparation = true
                  sucreStock -= 5
                  laitStock -= choix_dose * 0.05
                }
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                action = 0
              }
            }
            if(choix_sucre == 3) {
              if(sucreStock >= 10) {
                if(choix_lait == 1) {
                  println("Prix total : CHF 2.00 + CHF 0.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.50 + 0.20 + (choix_dose * 0.05)))
                  boisson_preparation = true
                  sucreStock -= 10
                  laitStock -= choix_dose * 0.05
                } else {
                  println("Prix total : CHF 2.50 + CHF 0.20 = CHF 2.70")
                  boisson_preparation = true
                  sucreStock -= 10
                  laitStock -= choix_dose * 0.05
                }
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                action = 0
              }
            }
            if(choix_sucre == 4) {
              if(sucreStock >= 15) {
                if(choix_lait == 1) {
                  println("Prix total : CHF 2.00 + CHF 0.30 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.00 + 0.30 + (choix_dose * 0.05)))
                  boisson_preparation = true
                  sucreStock -= 15
                  laitStock -= choix_dose * 0.05
                }else {
                  println("prix total : CHF 2.50 + CHF 0.30 = CHF 2.80")
                  boisson_preparation = true
                  sucreStock -= 15
                  laitStock -= choix_dose * 0.05
                }
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                action = 0
              }
            }
            cafeStock -= 6
            laitStock -= 0.1
          } else {
            if(cafeStock < 6) {
              println("Erreur : Quantité de poudre de café pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            if(laitStock < 0.1) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            action = 0
          }
        }
        if(choix_boisson == 3) {
          if(cafeStock >= 6 || laitStock >= 0.12) {
            println("Veuillez choisir la taille de votre Latte :")
            println("")
            println("1) Petit - CHF 2.70")
            println("2) Moyen - CHF 3.20")
            println("3) Grand - CHF 3.70")
            println("")
            print("> ")
            latte_taille_choix = readShort()

            println("Souhaitez-vous ajouter du sucre ?")
            println("")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            println("")
            print("> ")
            choix_sucre = readShort()
            println("")
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("")
            println("1) Oui")
            println("2) Non")
            println("")
            print("> ")
            choix_lait = readShort()
            if(choix_lait == 1) {
              while(choix_dose < 1 || choix_dose > 3) {
                println("Combien de doses ? (maximum 3 doses, 1 dose = 50ml)")
                print("> ")
                choix_dose = readShort()
                if(choix_dose < 1 || choix_dose > 3) {
                  println("Entrée non valide, veuillez réessayer.")
                }
              }
            }
            if(latte_taille_choix == 1) {
              if(choix_sucre == 1) {
                if(choix_lait == 1) {
                  println("Prix total : CHF 2.70 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.50 + (choix_dose * 0.05)))
                  boisson_preparation = true
                  laitStock -= choix_dose * 0.05
                } else {
                  println("Prix total : CHF 2.70")
                  boisson_preparation = true
                }
              }
              if(choix_sucre == 2) {
                if(sucreStock >= 5) {
                  sucreStock -= 5
                  if(choix_lait == 1) {
                    println("Prix total : CHF 2.70 + CHF 0.10 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.70 + 0.10 + (choix_dose * 0.05)))
                    boisson_preparation = true
                    laitStock -= choix_dose * 0.05
                  } else {
                    println("Prix total : CHF 2.70 + CHF 0.10 = CHF 2.80")
                    boisson_preparation = true
                  }
                } else {
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
              }
              if(choix_sucre == 3) {
                if(sucreStock >= 10) {
                  sucreStock -= 10
                  if(choix_lait == 1) {
                    println("Prix total : CHF 2.70 + CHF 0.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.70 + 0.20 + (choix_dose * 0.05)))
                    boisson_preparation = true
                    laitStock -= choix_dose * 0.05
                  } else {
                    println("Prix total : CHF 2.70 + CHF 0.20 = CHF 2.90")
                    boisson_preparation = true
                  }
                } else {
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
              }
              if(choix_sucre == 4) {
                if(sucreStock >= 15) {
                  sucreStock -= 15
                  if(choix_lait == 1) {
                    println("Prix total : CHF 2.70 + CHF 0.30 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.70 + 0.30 + (choix_dose * 0.05)))
                    boisson_preparation = true
                    laitStock -= choix_dose * 0.05
                  } else {
                    println("Prix total : CHF 2.70 + CHF 0.30 = CHF 3.00")
                    boisson_preparation = true
                  }
                } else {
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
              }
              cafeStock -= 6
              laitStock -= (0.12 + (choix_dose * 0.05))
            }
            if(latte_taille_choix == 2) {
              if(cafeStock >= 8 || laitStock >= 0.15) {
                if(choix_sucre == 1) {
                  if(choix_lait ==1) {
                    println("Prix total : CHF 3.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + (choix_dose * 0.05)))
                    boisson_preparation = true
                  } else {
                    println("Prix total : CHF 3.20")
                    boisson_preparation = true
                  }
                }
                if(choix_sucre == 2) {
                  if(sucreStock >= 5) {
                    sucreStock -= 5
                    if(choix_lait == 1) {
                      println("Prix total : CHF 3.20 + CHF 0.10 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + 0.10 + (choix_dose * 0.05)))
                      boisson_preparation = true
                      laitStock -= choix_dose * 0.05
                    } else {
                      println("Prix total : CHF 3.20 + CHF 0.10 = CHF 3.30")
                      boisson_preparation = true
                    }
                  } else {
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                    println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                }
                if(choix_sucre == 3) {
                  if(sucreStock >= 10) {
                    sucreStock -= 10
                    if(choix_lait == 1) {
                      println("Prix total : CHF 3.20 + CHF 0.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + 0.20 + (choix_dose * 0.05)))
                      boisson_preparation = true
                      laitStock -= choix_dose * 0.05
                    } else {
                      println("Prix total : CHF 3.20 + CHF 0.20 = CHF 3.40")
                      boisson_preparation = true
                    }
                  } else {
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                    println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                }
                if(choix_sucre == 4) {
                  if(sucreStock >= 15) {
                    sucreStock -= 15
                    if(choix_lait == 1) {
                      println("Prix total : CHF 3.20 + CHF 0.30 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + 0.30 + (choix_dose * 0.05)))
                      boisson_preparation = true
                      laitStock -= choix_dose * 0.05
                    } else {
                      println("Prix total : CHF 3.20 + CHF 0.30 = CHF 3.50")
                      boisson_preparation = true
                    }
                  } else {
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                    println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                }
              }
              cafeStock -= 8
              laitStock -= (0.15 + (choix_dose * 0.05))
            }
            if(latte_taille_choix == 3) {
              if(cafeStock >= 12 || laitStock >= 0.2) {
                if(choix_sucre == 1) {
                  if(choix_lait ==1) {
                    println("Prix total : CHF 3.70 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + (choix_dose * 0.05)).toDouble)
                    boisson_preparation = true
                  } else {
                    println("Prix total : CHF 3.70")
                    boisson_preparation = true
                  }
                }
                if(choix_sucre == 2) {
                  if(sucreStock >= 5) {
                    sucreStock -= 5
                    if(choix_lait == 1) {
                      println("Prix total : CHF 3.70 + CHF 0.10 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.70 + 0.10 + (choix_dose * 0.05)))
                      boisson_preparation = true
                      laitStock -= choix_dose * 0.05
                    } else {
                      println("Prix total : CHF 3.70 + CHF 0.10 = CHF 3.80")
                      boisson_preparation = true
                    }
                  } else {
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                    println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                }
                if(choix_sucre == 3) {
                  if(sucreStock >= 10) {
                    sucreStock -= 10
                    if(choix_lait == 1) {
                      println("Prix total : CHF 3.70 + CHF 0.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.70 + 0.20 + (choix_dose * 0.05)))
                      boisson_preparation = true
                      laitStock -= choix_dose * 0.05
                    } else {
                      println("Prix total : CHF 3.70 + CHF 0.20 = CHF 3.90")
                      boisson_preparation = true
                    }
                  } else {
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                    println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                }
                if(choix_sucre == 4) {
                  if(sucreStock >= 15) {
                    sucreStock -= 15
                    if(choix_lait == 1) {
                      println("Prix total : CHF 3.70 + CHF 0.30 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.70 + 0.30 + (choix_dose * 0.05)))
                      boisson_preparation = true
                      laitStock -= choix_dose * 0.05
                    } else {
                      println("Prix total : CHF 3.70 + CHF 0.30 = CHF 4.00")
                      boisson_preparation = true
                    }
                  } else {
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                    println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                }
              }
              cafeStock -= 12
              laitStock -= 0.2
            }
            action = 0
          } else {
            if(cafeStock < 6) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            if(laitStock < 0.12) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
            action = 0
          }
        }
        if(boisson_preparation == true) {
          println("")
          println("Veuillez payer en utilisant Twint.")
          println("Votre code de paiement est : " + word)
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
          action = 0
        }
        boisson_preparation = false
      }
      if(action == 2) {
        print("Mode Admin")
        println("")
        print("Entrez le code PIN : ****** ")
        println("")
        print("> ")
        EntreePIN = readLine()
        if(EntreePIN == AdminPIN) {
          Thread.sleep(1000)
          println("Accès autorisé.")
          println("")
          println("Stocks:")
          println("")
          println("   Poudre de café : " + cafeStock + "g")
          println("   Sucre          : " + sucreStock + "g")
          println("   Lait           : " + laitStock + "ml")
          println("")
          println("Réapprovisionnement des stocks...")
          print("Ajout : ")
          println("")
          print("   Poudre de café : ")
          var c = readShort()
          print("   Sucre          : ")
          var s = readShort()
          print("   Lait           : ")
          var l = readShort()
          println("Ajout de : " + c + "g de poudre de café, " + s + "g de sucre, et " + l + "ml de lait dans les stocks.")
          println("")
          Thread.sleep(1000)
          println("Niveaux de stocks mis à jour :")
          println("")
          println("   Poudre de café : " + (cafeStock + c) + "g")
          println("   Sucre          : " + (sucreStock + s) + "g")
          println("   Lait           : " + (laitStock + l) + "L")
          Thread.sleep(1000)
          println("")
          println("Retour au menu principal...")
          Thread.sleep(2000)
        } else {
          while(EntreePIN != AdminPIN) {
            println("")
            println("Code PIN incorrect. Veuillez réessayer.")
            println("")
            print("Entrez le code PIN : ******")
            println("")
            print("> ")
            EntreePIN = readLine()
          }
          if(EntreePIN == AdminPIN) {
            println("")
            println("Stocks:")
            println("")
            println("   Poudre de café : " + cafeStock + "g")
            println("   Sucre          : " + sucreStock + "g")
            println("   Lait           : " + laitStock + "L")
            println("")
            println("Entrez les quantités à ajouter :")
            print("Poudre de café : ")
            var c = readLong()
            print("Sucre          : ")
            var s = readLong()
            print("Lait           : ")
            var l = readDouble()
            println("")
            println("Réapprovisionnement des stocks...")
            println("")
            println("Ajout de : " + c + "g de poudre de café, " + s + "g de sucre, and " + l + "ml de lait dans les stocks.")
            println("")
            println("Niveaux de stocks mis à jour :")
            println("")
            println("Poudre de café : " + (cafeStock + c) + "g")
            println("Sucre          : " + (sucreStock + s) + "g")
            println("Lait           : " + (laitStock + l) + "L")
            cafeStock += c
            sucreStock += s
            laitStock += l
            Thread.sleep(1000)
            println("")
            println("Retour au menu principal...")
            Thread.sleep(2000)
          }
        }
        action = 0
      }
      if(action == 3) {
        stayinloop = false
        println("Au revoir !")
        println("")
      }
    }
  }
}