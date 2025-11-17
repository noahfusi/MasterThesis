import scala.util.Random
object Main {
  var PCstock: Int = 50 // (g), PC: poudre de café
  var Sucrestock: Int = 30 // (g)
  var Laitstock: Double = 0.5 // (L), stocks initiaux des ingrédients
  var prixStotale: Double = 0.0 // Sous-total
  var prixLait: Double = 0.0 // 0.05 CHF par dose
  var prixSucre: Double = 0.0 // 0.10 CHF par option
  val codePIN = "434343" // Default PIN code

  def generateRandomCode(length: Int): String = {
    val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    val code = (1 to length).map(_ => chars(scala.util.Random.nextInt(chars.length))).mkString
    code
  }
  def main(args: Array[String]): Unit = {
    var continue = true
    while (continue) {
      println("Veuillez sélectionner votre mode:")
      println("1. Client")
      println("2. Admin")
      println("3. Quitter")
      print(">")

      val mode = scala.io.StdIn.readLine()
      mode match {
        case "1" =>
          println("Mode Client: Veuillez sélectionner votre boisson: ")
          println("1. Expresso - CHF 2.00")
          println("2. Cappuccino - CHF 2.50")
          println("3. Latte - les prix varient selon la taille")
          print(">")

          val choixboisson = scala.io.StdIn.readLine()
          choixboisson match {
            case "1" =>
              if (PCstock >= 8) {
                PCstock -= 8
                println("Boisson sélectionnée : Expresso")
                prixStotale = 2.00

                println("Souhaitez-vous ajouter du sucre en supplément ?")
                println("1. Oui")
                println("2. Non")
                print(">")
                val sucresupp = scala.io.StdIn.readLine()
                sucresupp match {
                  case "1" =>
                    println("Veuillez choisir la quantité du sucre :")
                    println("1. Peu (5g)")
                    println("2. Moyen (10g)")
                    println("3. Beaucoup (15g)")
                    print(">")

                    val choixSucre = scala.io.StdIn.readLine()
                    choixSucre match {
                      case "1" =>
                        if (Sucrestock >= 5) {
                          Sucrestock -= 5
                          prixStotale += 0.1
                          println("5g de sucre ajouté.")
                        } else {
                          println("Quantité de sucre insuffisante.")
                        }
                      case "2" =>
                        if (Sucrestock >= 10) {
                          Sucrestock -= 10
                          prixStotale += 0.2
                          println("10g de sucre ajouté.")
                        } else {
                          println("Quantité de sucre insuffisante.")
                        }
                      case "3" =>
                        if (Sucrestock >= 15) {
                          Sucrestock -= 15
                          prixStotale += 0.3
                          println("15g de sucre ajouté.")
                        } else {
                          println("Quantité de sucre insuffisante.")
                        }
                      case _ =>
                        println("Option invalide.")
                    }
                  case "2" =>
                    println("Pas de sucre ajouté.")
                }

                println("Souhaitez-vous ajouter du lait en supplément ?")
                println("1. Oui")
                println("2. Non")
                print(">")
                val laitsupp = scala.io.StdIn.readLine()
                laitsupp match {
                  case "1" =>
                    println("Veuillez choisir la quantité de lait :")
                    println("1. 1 dose (0.05L)")
                    println("2. 2 doses (0.10L)")
                    println("3. 3 doses (0.15L)")
                    print(">")

                    val laitchoix = scala.io.StdIn.readLine()
                    laitchoix match {
                      case "1" =>
                        if (Laitstock >= 0.05) {
                          Laitstock -= 0.05
                          prixStotale += 0.05
                          println("1 dose de lait ajouté.")
                        } else {
                          println("Quantité de lait insuffisante.")
                        }
                      case "2" =>
                        if (Laitstock >= 0.10) {
                          Laitstock -= 0.10
                          prixStotale += 0.10
                          println("2 doses de lait ajouté.")
                        } else {
                          println("Quantité de lait insuffisante.")
                        }
                      case "3" =>
                        if (Laitstock >= 0.15) {
                          Laitstock -= 0.15
                          prixStotale += 0.15
                          println("3 doses de lait ajouté.")
                        } else {
                          println("Quantité de lait insuffisante.")
                        }
                      case _ =>
                        println("Option invalide.")
                    }
                  case "2" =>
                    println("Pas de lait ajouté.")
                }

                val prixStotaleRounded = BigDecimal(prixStotale).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
                println("Prix total: CHF " + prixStotaleRounded)

              } else {
                println("Stock insuffisant, veuillez choisir une autre boisson.")
              }

            case "2" =>
              if (PCstock >= 6 && Laitstock >= 0.15) {
                PCstock -= 6
                Laitstock -= 0.15
                println("Boisson sélectionnée : Cappuccino")
                prixStotale = 2.50

                // Sucre
                println("Souhaitez-vous ajouter du sucre en supplément ?")
                println("1. Oui")
                println("2. Non")
                print(">")
                val sucresupp = scala.io.StdIn.readLine()
                sucresupp match {

                  case "1" =>
                    println("Veuillez choisir la quantité du sucre :")
                    println("1. Peu (5g)")
                    println("2. Moyen (10g)")
                    println("3. Beaucoup (15g)")
                    print(">")

                    val choixSucre = scala.io.StdIn.readLine()
                    choixSucre match {
                      case "1" =>
                        if (Sucrestock >= 5) {
                          Sucrestock -= 5
                          prixStotale += 0.1
                          println("5g de sucre ajouté.")
                        } else {
                          println("Quantité de sucre insuffisante.")
                        }
                      case "2" =>
                        if (Sucrestock >= 10) {
                          Sucrestock -= 10
                          prixStotale += 0.2
                          println("10g de sucre ajouté.")
                        } else {
                          println("Quantité de sucre insuffisante.")
                        }
                      case "3" =>
                        if (Sucrestock >= 15) {
                          Sucrestock -= 15
                          prixStotale += 0.3
                          println("15g de sucre ajouté.")
                        } else {
                          println("Quantité de sucre insuffisante.")
                        }
                      case _ =>
                        println("Option invalide.")
                    }
                  case "2" =>
                    println("Pas de sucre ajouté.")
                }


                println("Souhaitez-vous ajouter du lait en supplément?")
                println("1. Oui")
                println("2. Non")
                print(">")
                val laitsupp = scala.io.StdIn.readLine()
                laitsupp match {
                  case "1" =>
                    println("Veuillez choisir la quantité de lait:")
                    println("1. 1 dose (0.05L)")
                    println("2. 2 doses (0.10L)")
                    println("3. 3 doses (0.15L)")
                    print(">")

                    val laitchoix = scala.io.StdIn.readLine()
                    laitchoix match {
                      case "1" =>
                        if (Laitstock >= 0.05) {
                          Laitstock -= 0.05
                          prixStotale += 0.05
                          println("1 dose de lait ajouté.")
                        } else {
                          println("Quantité de lait insuffisante.")
                        }
                      case "2" =>
                        if (Laitstock >= 0.10) {
                          Laitstock -= 0.10
                          prixStotale += 0.10
                          println("2 doses de lait ajouté.")
                        } else {
                          println("Quantité de lait insuffisante.")
                        }
                      case "3" =>
                        if (Laitstock >= 0.15) {
                          Laitstock -= 0.15
                          prixStotale += 0.15
                          println("3 doses de lait ajouté")
                        } else {
                          println("Quantité de lait insuffisante.")
                        }
                      case _ =>
                        println("Option invalide.")
                    }
                  case "2" =>
                    println("Pas de lait ajouté.")
                }

                println("Prix total: CHF " + prixStotale)
              } else {
                println("Stock insuffisant, veuillez choisir une autre boisson.")
              }
            case "3" =>
              println("Boisson sélectionnée: Latte")
              println("Veuillez choisir une taille: ")
              println("1. Petit - CHF 2.70")
              println("2. Moyen - CHF 3.20")
              println("3. Grand - CHF 3.70")
              print(">")

              val lattetaille = scala.io.StdIn.readLine()
              lattetaille match {
                case "1" =>
                  if (PCstock >= 6 && Laitstock >= 0.12) {
                    PCstock -= 6
                    Laitstock -= 0.12
                    prixStotale = 2.70
                    println("Petit Latte sélectionné.")
                  } else {
                    println("Stock insuffisant pour un petit latte.")
                  }
                case "2" =>
                  if (PCstock >= 8 && Laitstock >= 0.16) {
                    PCstock -= 8
                    Laitstock -= 0.16
                    prixStotale = 3.20
                    println("Moyen Latte sélectionné.")
                  } else {
                    println("Stock insuffisant pour un moyen latte.")
                  }
                case "3" =>
                  if (PCstock >= 10 && Laitstock >= 0.20) {
                    PCstock -= 10
                    Laitstock -= 0.20
                    prixStotale = 3.70
                    println("Grand Latte sélectionné.")
                  } else {
                    println("Stock insuffisant pour un grand latte.")
                  }
                case _ =>
                  println("Option invalide.")
              }


              println("Souhaitez-vous ajouter du sucre en supplément?") // 설탕
              println("1. Oui")
              println("2. Non")
              print(">")
              val sucresupp = scala.io.StdIn.readLine()
              sucresupp match {
                case "1" =>
                  println("Veuillez choisir la quantité du sucre:")
                  println("1. Peu (5g) - CHF 0.1")
                  println("2. Moyen (10g) - CHF 0.2")
                  println("3. Beaucoup (15g) - CHF 0.3")
                  print(">")

                  val choixSucre = scala.io.StdIn.readLine()
                  choixSucre match {
                    case "1" =>
                      if (Sucrestock >= 5) {
                        Sucrestock -= 5
                        prixStotale += 0.1
                        println("5g de sucre ajouté.")
                      } else {
                        println("Quantité de sucre insuffisante.")
                      }
                    case "2" =>
                      if (Sucrestock >= 10) {
                        Sucrestock -= 10
                        prixStotale += 0.2
                        println("10g de sucre ajouté.")
                      } else {
                        println("Quantité de sucre insuffisante.")
                      }
                    case "3" =>
                      if (Sucrestock >= 15) {
                        Sucrestock -= 15
                        prixStotale += 0.3
                        println("15g de sucre ajouté.")
                      } else {
                        println("Quantité de sucre insuffisante.")
                      }
                    case _ =>
                      println("Option invalide.")
                  }
                case "2" =>
                  println("Pas de sucre ajouté.")
              }


              println("Souhaitez-vous ajouter du lait en supplément ?")
              println("1. Oui")
              println("2. Non")
              print(">")
              val laitsupp = scala.io.StdIn.readLine()
              laitsupp match {
                case "1" =>
                  println("Veuillez choisir la quantité de lait:")
                  println("1. 1 dose (0.05L) - CHF 0.05")
                  println("2. 2 doses (0.10L) - CHF 0.1")
                  println("3. 3 doses (0.15L) - CHF 0.15")
                  print(">")

                  val laitchoix = scala.io.StdIn.readLine()
                  laitchoix match {
                    case "1" =>
                      if (Laitstock >= 0.05) {
                        Laitstock -= 0.05
                        prixStotale += 0.05
                        println("1 dose de lait ajouté.")
                      } else {
                        println("Quantité de lait insuffisante.")
                      }
                    case "2" =>
                      if (Laitstock >= 0.10) {
                        Laitstock -= 0.10
                        prixStotale += 0.10
                        println("2 doses de lait ajouté.")
                      } else {
                        println("Quantité de lait insuffisante.")
                      }
                    case "3" =>
                      if (Laitstock >= 0.15) {
                        Laitstock -= 0.15
                        prixStotale += 0.15
                        println("3 doses de lait ajouté.")
                      } else {
                        println("Quantité de lait insuffisante.")
                      }
                    case _ =>
                      println("Option invalide.")
                  }
                case "2" =>
                  println("Pas de lait ajouté.")
              }

              println("Prix total: CHF " + prixStotale)

          }
          if (prixStotale > 0) {
            println("Veuillez payer en utilisant Twint.")
            val codeTwint = generateRandomCode(5)
            println("Votre code de paiement est : codeTwint")
            println("(En attente de validation du paiement...)")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté.")
            println("Préparation de votre boisson...")
            Thread.sleep(3000)
            println(s"Votre boisson est prête ! Bonne dégustation !")
          }
        case "2" =>
          println("Mode Admin: un code PIN a 6 chiffres ")
          val mdpentrer = scala.io.StdIn.readLine() //mdp = mot de passe
          if (mdpentrer == codePIN) {
            println("Accès autorisé.")
            println("Stock:")
            println("Poudre de café:" + PCstock + " g")
            println("Sucre:" + Sucrestock + " g")
            println("Lait:" + Laitstock + " L")
            println("Voulez-vous réaprovisionner les stocks ?")
            println("1. Oui")
            println("2. Non")
            print(">")

            val reapprovisionner = scala.io.StdIn.readLine()
            if (reapprovisionner == "1") {
              // Réapprovisionnement
              println("Quelle quantité de poudre de café voulez-vous ajouter?")
              val cafeAjout = scala.io.StdIn.readInt()
              PCstock += cafeAjout
              Thread.sleep(3000)
              println("Poudre de café réapprovisionnée. Nouveau de stock mis-à-jour: " + PCstock + " g")

              println("Quelle quantité de sucre voulez-vous ajouter?")
              val sucreAjout = scala.io.StdIn.readInt()
              Sucrestock += sucreAjout
              Thread.sleep(3000)
              println("Sucre réapprovisionné. Nouveau de stock mis-à-jour: " + Sucrestock + " g")

              println("Quelle quantité de lait voulez-vous ajouter (L) ?")
              val laitAjout = scala.io.StdIn.readDouble()
              Laitstock += laitAjout
              Thread.sleep(3000)
              println("Lait réapprovisionné. Nouveau de stock mis-à-jour: " + Laitstock + " L")
            } else {
              println("Aucune modification des stocks.")
            }
            println("Retour au menu principal...")
          } else {
            println("Code PIN incorrect. Accès refusée.")
          }
        case "3" =>
          println("Merci d'avoir utilisé notre machine. Au revoir !")
          continue = false
      }
    }
  }
}
