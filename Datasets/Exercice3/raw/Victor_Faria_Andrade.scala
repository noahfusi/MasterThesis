import scala.io.StdIn._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import scala.io.Source

class Machine(val id: Int, var pincode: String, var lait: Int, var sucre: Int, var cafe: Int) {

  def ajouterIngredient(ingredient: String, quantite: Int): Unit = {
    ingredient.toLowerCase match {
      case "cafe" => cafe = cafe + quantite
      case "sucre"  => sucre = sucre + quantite
      case "lait"   => lait = lait + quantite
      case _        => println("Ingrédient non reconnu par la machine.")
    }
  }
  def retirerIngredient(ingredient: String, quantite: Int): Boolean = {
    ingredient.toLowerCase match {
      case "cafe" if cafe >= quantite =>
        cafe = cafe - quantite
        true
      case "sucre" if sucre >= quantite =>
        sucre = sucre - quantite
        true
      case "lait" if lait >= quantite =>
        lait = lait - quantite
        true
      case _ =>
        println("Impossible de retirer " + quantite + " de " + ingredient + ". Stock insuffisant dans la machine ou ingrédient non reconnu par la machine.")
        false
    }
  }
}


object Nospresso {

  val machines = ArrayBuffer[Machine]()
  val fichier = "machines.csv"

  def afficherDetailsMachine(machine: Machine): Unit = {
    println("Machine " + machine.id + " :")
    println("  Code PIN : " + machine.pincode)
    println("  Lait : " + (machine.lait / 1000.0) + "L")
    println("  Sucre : " + machine.sucre + "g")
    println("  Café : " + machine.cafe + "g")
    println("")
  }
  def afficherToutesMachines(machines: ArrayBuffer[Machine]): Unit = {
    println("Liste des machines disponibles :")
    machines.foreach(afficherDetailsMachine)
  }
  def chargementDepuisFichier(filename: String): ArrayBuffer[Machine] = {
    var listeMachines = ArrayBuffer[Machine]()
    println("Chargement des machines depuis le fichier " + fichier + "...")
    println("")
    try {
      val source = Source.fromFile(fichier)
      val lignes = source.getLines().drop(1)

      for (ligne <- lignes) {
        val elements = ligne.split(",")
        if (elements.length == 4) {
          val machine = new Machine(
            id = listeMachines.size + 1,
            pincode = elements(0),
            lait = elements(1).toInt,
            sucre = elements(2).toInt,
            cafe = elements(3).toInt
          )
          listeMachines = listeMachines :+ machine
          afficherDetailsMachine(machine)
        }
      }
      source.close()
      println(listeMachines.size + " machine(s) chargée(s).")
    } catch {
      case _: java.io.FileNotFoundException =>
        println("Erreur : Le fichier " + fichier + " est introuvable. Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        System.exit(0)
    }
    listeMachines
  }
  def sauvegardeDansFichier(filename: String,machines: ArrayBuffer[Machine]): Unit = {
    println("Sauvegarde des machines dans le fichier " + fichier + "...")
    try {
      val ecriture = new PrintWriter(fichier)
      ecriture.println("PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach { machine =>
        ecriture.println(machine.pincode + "," + machine.lait + "," + machine.sucre + "," + machine.cafe)
      }
      ecriture.close()
      println(machines.size + " machine(s) sauvegardée(s).")
    } catch {
      case _: java.io.IOException =>
        println("Erreur : Échec de l'écriture dans "+ fichier)
        println("Le fichier peut être verrouillé ou en lecture seule.")
        println("Erreur : Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        System.exit(0)
    }
    println("Fichier sauvegardé avec succès.")
    println("Au revoir !")
    println("")
  }
  def selectionMachine(machines: ArrayBuffer[Machine]): Option[Machine] = {
    var choixValide = false
    var choix: Int = -1

    while (!choixValide) {
      println("")
      println("Veuillez sélectionner une machine (1-" + machines.size +") : ")
      print("> ")
      choix = readInt()

      if (choix > 0 && choix <= machines.size) {
        choixValide = true
      } else {
        println("Choix invalide. Veuillez entrer un numéro entre 1 et " + machines.size + ".")
      }
    }
    Some(machines(choix - 1))
  }
  def pin(): String = {
    Random.alphanumeric.take(5).mkString
  }
  def validatePin(machine: Machine): Boolean = {
    var essais = 0
    var essaisPin = ""
    while (essais < 3) {
      println("Entrez le code PIN : ")
      print("> ")
      essaisPin = readLine()
      if (essaisPin == machine.pincode) {
        println("Accès accordé à la Machine " + machine.id + ".")
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
  def updatePin(machine: Machine): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres > ")
    var nouveauPin = readLine()
    while (nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit)) {
      println("Le code PIN ne contient pas 6 chiffres > ")
      nouveauPin = readLine()
    }
    machine.pincode = nouveauPin
    println("Le code PIN pour la machine " + machine.id + " a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }
  def serveClient(machine: Machine): Boolean = {
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
      if(machine.cafe >= 8) {
        if(choix_sucre == 1) {
          println("Prix total : CHF 2.00")
        }
        if(choix_sucre == 2) {
          if(machine.sucre >= 5) {
            machine.retirerIngredient("sucre", 5)
            println("Prix total : CHF 2.00 + CHF 0.10 = CHF 2.10")
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            return false
          }
        }
        if(choix_sucre == 3) {
          if(machine.sucre >= 10) {
            machine.retirerIngredient("sucre", 10)
            println("Prix total : CHF 2.00 + CHF 0.20 = CHF 2.20")
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            return false
          }
        }
        if(choix_sucre == 4) {
          if(machine.sucre >= 15) {
            machine.retirerIngredient("sucre", 15)
            println("Prix total : CHF 2.00 + CHF 0.30 = CHF 2.30")
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            return false
          }
        }
        machine.retirerIngredient("cafe", 8)
      } else {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        return false
      }
    }
    if(choix_boisson == 2) {
      if(machine.cafe >= 6 && machine.lait >= 100) {
        if(choix_sucre == 1) {
          if(choix_lait == 1) {
            println("Prix total : CHF 2.50 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.50 + (choix_dose * 0.05)))
            machine.retirerIngredient("lait", choix_dose * 50)
          } else {
            println("Prix total : CHF 2.50")
            machine.retirerIngredient("lait", choix_dose * 50)
          }
        }
        if(choix_sucre == 2) {
          if(machine.sucre >= 5) {
            if(choix_lait == 1) {
              println("Prix total : CHF 2.00 + CHF 0.10 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.50 + 0.10 + (choix_dose * 0.05)))
              machine.retirerIngredient("sucre", 5)
              machine.retirerIngredient("lait", choix_dose * 50)
            } else {
              println("Prix total : CHF 2.50 + CHF 0.10 = CHF 2.60")
              machine.retirerIngredient("sucre", 5)
              machine.retirerIngredient("lait", choix_dose * 50)
            }
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            return false
          }
        }
        if(choix_sucre == 3) {
          if(machine.sucre >= 10) {
            if(choix_lait == 1) {
              println("Prix total : CHF 2.50 + CHF 0.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.50 + 0.20 + (choix_dose * 0.05)))
              machine.retirerIngredient("sucre", 10)
              machine.retirerIngredient("lait", choix_dose * 50)
            } else {
              println("Prix total : CHF 2.50 + CHF 0.20 = CHF 2.70")
              machine.retirerIngredient("sucre", 10)
              machine.retirerIngredient("lait", choix_dose * 50)
            }
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            return false
          }
        }
        if(choix_sucre == 4) {
          if(machine.sucre >= 15) {
            if(choix_lait == 1) {
              println("Prix total : CHF 2.50 + CHF 0.30 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.50 + 0.30 + (choix_dose * 0.05)))
              machine.retirerIngredient("sucre", 15)
              machine.retirerIngredient("lait", choix_dose * 50)
            }else {
              println("prix total : CHF 2.50 + CHF 0.30 = CHF 2.80")
              machine.retirerIngredient("sucre", 15)
              machine.retirerIngredient("lait", choix_dose * 50)
            }
          } else {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            return false
          }
        }
        machine.retirerIngredient("cafe", 6)
        machine.retirerIngredient("lait", 100)

      } else {
        if(machine.cafe < 6) {
          println("Erreur : Quantité de poudre de café pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if(machine.lait < 100) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        return false
      }
    }
    if(choix_boisson == 3) {
      if(machine.cafe>= 6 && machine.lait >= 120) {
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
              machine.retirerIngredient("lait", choix_dose * 50)
            } else {
              println("Prix total : CHF 2.70")
            }
          }
          if(choix_sucre == 2) {
            if(machine.sucre >= 5) {
              machine.retirerIngredient("sucre", 5)
              if(choix_lait == 1) {
                println("Prix total : CHF 2.70 + CHF 0.10 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.70 + 0.10 + (choix_dose * 0.05)))
                machine.retirerIngredient("lait", choix_dose * 50)
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
            if(machine.sucre >= 10) {
              machine.retirerIngredient("sucre", 10)
              if(choix_lait == 1) {
                println("Prix total : CHF 2.70 + CHF 0.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.70 + 0.20 + (choix_dose * 0.05)))
                machine.retirerIngredient("lait", choix_dose * 50)
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
            if(machine.sucre >= 15) {
              machine.retirerIngredient("sucre", 15)
              if(choix_lait == 1) {
                println("Prix total : CHF 2.70 + CHF 0.30 + CHF " + (choix_dose * 0.05) + " = CHF " + (2.70 + 0.30 + (choix_dose * 0.05)))
                machine.retirerIngredient("lait", choix_dose * 50)
              } else {
                println("Prix total : CHF 2.70 + CHF 0.30 = CHF 3.00")
              }
            } else {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              return false
            }
          }
          machine.retirerIngredient("cafe", 6)
          machine.retirerIngredient("lait", 120)
        }
        if(latte_taille_choix == 2) {
          if(machine.cafe >= 8 && machine.lait >= 150) {
            if(choix_sucre == 1) {
              if(choix_lait ==1) {
                println("Prix total : CHF 3.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + (choix_dose * 0.05)))
              } else {
                println("Prix total : CHF 3.20")
              }
            }
            if(choix_sucre == 2) {
              if(machine.sucre >= 5) {
                machine.retirerIngredient("sucre", 5)
                if(choix_lait == 1) {
                  println("Prix total : CHF 3.20 + CHF 0.10 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + 0.10 + (choix_dose * 0.05)))
                  machine.retirerIngredient("lait", choix_dose * 50)
                } else {
                  println("Prix total : CHF 3.20 + CHF 0.10 = CHF 3.30")
                }
              } else {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
            }
            if(choix_sucre == 3) {
              if(machine.sucre >= 10) {
                machine.retirerIngredient("sucre", 10)
                if(choix_lait == 1) {
                  println("Prix total : CHF 3.20 + CHF 0.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + 0.20 + (choix_dose * 0.05)))
                  machine.retirerIngredient("lait", choix_dose * 50)
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
              if(machine.sucre >= 15) {
                machine.retirerIngredient("sucre", 15)
                if(choix_lait == 1) {
                  println("Prix total : CHF 3.20 + CHF 0.30 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.20 + 0.30 + (choix_dose * 0.05)))
                  machine.retirerIngredient("lait", choix_dose * 50)
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
          machine.retirerIngredient("cafe", 8)
          machine.retirerIngredient("lait", 150)
        }
        if(latte_taille_choix == 3) {
          if(machine.cafe>= 12 && machine.lait >= 200) {
            if(choix_sucre == 1) {
              if(choix_lait ==1) {
                println("Prix total : CHF 3.70 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.70 + (choix_dose * 0.05)).toDouble)
              } else {
                println("Prix total : CHF 3.70")
              }
            }
            if(choix_sucre == 2) {
              if(machine.sucre >= 5) {
                machine.retirerIngredient("sucre", 5)
                if(choix_lait == 1) {
                  println("Prix total : CHF 3.70 + CHF 0.10 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.70 + 0.10 + (choix_dose * 0.05)))
                  machine.retirerIngredient("lait", choix_dose * 50)
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
              if(machine.sucre >= 10) {
                machine.retirerIngredient("sucre", 10)
                if(choix_lait == 1) {
                  println("Prix total : CHF 3.70 + CHF 0.20 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.70 + 0.20 + (choix_dose * 0.05)))
                  machine.retirerIngredient("lait", choix_dose * 50)
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
              if(machine.sucre >= 15) {
                machine.retirerIngredient("sucre", 15)
                if(choix_lait == 1) {
                  println("Prix total : CHF 3.70 + CHF 0.30 + CHF " + (choix_dose * 0.05) + " = CHF " + (3.70 + 0.30 + (choix_dose * 0.05)))
                  machine.retirerIngredient("lait", choix_dose * 50)
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
          machine.retirerIngredient("cafe", 12)
          machine.retirerIngredient("lait", 200)
        }
      } else {
        if(machine.cafe < 6) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
        if(machine.lait < 120) {
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
  def restockMachine(machine: Machine): Unit = {
    Thread.sleep(1000)
    println("")
    println("Niveaux de stock actuels:")
    var lait_stock = machine.lait.toDouble/1000
    println("")
    println("   Poudre de café : " + machine.cafe + "g")
    println("   Sucre          : " + machine.sucre + "g")
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
    println("Niveaux de stocks mis à jour pour la machine " + machine.id + " :")
    println("")
    println("   Poudre de café : " + (machine.cafe + c) + "g")
    machine.ajouterIngredient("cafe",c)
    println("   Sucre          : " + (machine.sucre + s) + "g")
    machine.ajouterIngredient("sucre",s)
    println("   Lait           : " + (lait_stock + l) + "L")
    machine.ajouterIngredient("lait",(l*1000).toInt)
    Thread.sleep(1000)
    println("")
    println("Les stocks ont été mis à avec succès.")
    println("Retour au menu principal...")
    Thread.sleep(2000)
  }
  def main(args: Array[String]): Unit = {

    machines ++= chargementDepuisFichier(fichier)
    var stayinloop = true
    var action: Int = 0

    while (stayinloop) {
      var machineSelectionnee: Option[Machine] = None
      while (machineSelectionnee.isEmpty) {
        machineSelectionnee = selectionMachine(machines)
      }
      val machine = machineSelectionnee.get
      println("Vous avez sélectionné la machine " + machine.id)

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
        if (machine.id >= 0 && machine.id < machines.size) {
          if (serveClient(machine)) {
            println("Boisson préparée avec succès !")
          } else {
            println("Erreur dans la préparation de la boisson, veuillez vérifier les stocks.")
          }
        }
      }
      else if (action == 2) {
        println("Mode Admin")
        if (machine.id >= 0 && machine.id < machines.size) {
          if (validatePin(machine)) {
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
              updatePin(machine)
            } else  {
              restockMachine(machine)
            }
          }
        } else {
          stayinloop = false
          sauvegardeDansFichier(fichier,machines)
        }
      }
    }
  }
}