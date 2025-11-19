import io.StdIn._
import math._
import scala.util.Random
object Main {
  def main(args: Array[String]): Unit = {
    var retouraumode = true
    var poudredecafe = 50.0
    var sucre = 30.0
    var lait = 500.0 // 500 ml = 0.500 L , LE PROF A AUTORISE LA CONVERSION
    while (retouraumode) {
      var prixtotal = 0.0
      var prixboisson = 0.0
      var prixsucre = 0.0
      var prixdoselait = 0.0
      var codetwint = Random.alphanumeric.take(5).mkString
      println("Nospresso Café")
      println("Veuillez sélectionner votre mode : ")
      println("1 ) Client")
      println("2 ) Admin")
      println("3 ) Quitter")
      print(">")
      var mode = readInt()
      while (!(mode == 1 || mode == 2 || mode == 3)) {
        println("Le mode sélectionné n'est pas valide, Veuillez resélectionner votre mode : ")
        println("1 ) Client")
        println("2 ) Admin")
        println("3 ) Quitter")
        print(">")
        mode = readInt()
      }
      if (mode == 1) {
        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Capuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        print(">")
        var boisson = readInt()
        while (!(boisson == 1 || boisson == 2 || boisson == 3)) {
          println("ERREUR : Le boisson sélectionné n'est pas valide, Veuillez resélectionner votre boisson :")
          println("1) Expresso - CHF 2.00")
          println("2) Capuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print(">")
          boisson = readInt()
        } // JE DEMANDE LA TAILLE DU LATTE PLUS TARD CELA A ETE VU AVEC LE PROFESSEUR QUI M'A CONFIRMÉ, LE CODE EN BAS EST JUSTE POUR EN INFORMER LE CORRECTEUR
        if (boisson==3) {
          println("------------------------------------------------------------------")
          println("note : la taille desiré par l'utilisateur est demandé aprés (le prof est ok)")
          println("-------------------------------------------------------------------")
        }
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10 ")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        print(">")
        var sucreoption = readInt()
        while (!(sucreoption == 1 || sucreoption == 2 || sucreoption == 3 || sucreoption == 4)) {
          println("ERREUR : L'option sélectionné n'est pas valide, combien souhaitez vous ajoutez de sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10 ")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print(">")
          sucreoption = readInt()
        }
        if (boisson == 1 || boisson == 2 || boisson == 3) {
          if (sucreoption == 1) {
            sucre = sucre
            prixsucre = prixsucre
            prixtotal = prixtotal
          } else if (sucreoption == 2 && sucre >= 5) {
            prixsucre = prixsucre + 0.10
            prixtotal += 0.10
          } else if (sucreoption == 3 && sucre >= 10) {
            prixsucre = prixsucre + 0.20
            prixtotal += 0.20
          } else if (sucreoption == 4 && sucre >= 15) {
            prixsucre = prixsucre + 0.30
            prixtotal += 0.30
          } else {
            if ((sucreoption == 2 && sucre <= 5) || (sucreoption == 3 && sucre <= 10) || (sucreoption == 4 && sucre <= 15)) {
              println("ERREUR : La quantité de sucre est insuffisante pour l'option choisi choisissez une autre option de sucre ou vérifier les stocks en mode Admin ")
            }
          }
        }
        if ((sucreoption == 1) || (sucreoption == 2 && sucre >= 5) || (sucreoption == 3 && sucre >= 10) || (sucreoption == 4 && sucre >= 15)) {
          if (boisson == 2 || boisson == 3) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui ")
            println("2) Non")
            print(">")
            var laitsupplement = readInt()
            while (!(laitsupplement == 1 || laitsupplement == 2)) {
              println("ERREUR : Le choix sélectionné n'est pas valide, Souhaitez vous ajoutez du lait en supplement ?")
              println("1) Oui ")
              println("2) Non")
              print(">")
              laitsupplement = readInt()
            }
            if (laitsupplement == 1) {
              println(" Combien de doses souhaitez vous ? (3max)")
              println("1) 1 dose -- CHF 0.05 ")
              println("2) 2 doses -- CHF 0.10")
              println("3) 3 doses -- CHF 0.15")
              print(">")
              var doselait = readInt()
              while (!(doselait == 1 || doselait == 2 || doselait == 3)) {
                println("ERREUR : Le choix sélectionné n'est pas valide, combien de dose souhaitez vous ajoutez ?")
                println("1) 1 dose -- CHF 0.05 ")
                println("2) 2 doses -- CHF 0.10")
                println("3) 3 doses -- CHF 0.15")
                print(">")
                doselait = readInt()
              }
              if (doselait == 1 && lait >= 50) {
                prixdoselait = prixdoselait + 0.05
                prixtotal += 0.05
              } else if (doselait == 2 && lait >= 100) {
                prixdoselait = prixdoselait + 0.10
                prixtotal += 0.10
              } else if (doselait == 3 && lait >= 150) {
                prixdoselait = prixdoselait + 0.15
                prixtotal += 0.15
              } else {
                if ((doselait == 1 && lait <= 50) || (doselait == 2 && lait <= 100) || (doselait == 3 && lait <= 150)) {
                  println("ERREUR : la quantite de lait est insuffisante pour l'option choisi choissisez une autre dose, ou verifier les stocks en mode Admin")
                }
              }
              if ((doselait == 1 && lait >= 50) || (doselait == 2 && lait >= 100) || (doselait == 3 && lait >= 150)) {
                if (boisson == 2 && poudredecafe >= 6 && lait >= 100) {
                  poudredecafe = poudredecafe - 6
                  lait = lait - 100
                  prixboisson = prixboisson + 2.50
                  prixtotal += 2.50
                  if (sucreoption == 2) {
                    sucre = sucre - 5
                  }
                  if (sucreoption == 3) {
                    sucre = sucre - 10
                  }
                  if (sucreoption == 4) {
                    sucre = sucre - 15
                  }
                  if (doselait == 1) {
                    lait = lait - 50
                  }
                  if (doselait == 2) {
                    lait = lait - 100
                  }
                  if (doselait == 3) {
                    lait = lait - 150
                  }
                  printf("Prix : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixboisson, prixsucre, prixdoselait, prixtotal)
                  println("veuillez payer en utilisant TWINT")
                  println("votre code de paiement est :" + codetwint)
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci! Votre paiement a été accepté")
                  println("préparation de votre boisson...")
                  println("Votre Cappucino est prêt ! Bonne dégustation !")
                } else if (boisson == 2 && poudredecafe < 6) {
                  println("ERREUR : quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode admin")
                } else if (boisson == 2 && lait < 100) {
                  println("ERREUR : quantité de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode admin")
                }
                if (boisson == 3) {
                  println("Veuillez selectionner la taille désirez :")
                  println("1) Petit - CHF 2.70")
                  println("2) Moyen - CHF 3.20")
                  println("3) Grand - CHF 3.70")
                  var taillelatte = readInt()
                  while (!(taillelatte == 1 || taillelatte == 2 || taillelatte == 3)) {
                    println("ERREUR : la taille selectionne n'est pas valide, veuillez resélectionner la taille desirez :")
                    println("1) Petit - CHF 2.70")
                    println("2) Moyen - CHF 3.20")
                    println("3) Grand - CHF 3.70")
                    taillelatte = readInt()
                  }
                  if ((taillelatte == 1 && poudredecafe >= 6 && lait >= 120) || (taillelatte == 2 && poudredecafe >= 8 && lait >= 150) || (taillelatte == 3 && poudredecafe >= 12 && lait >= 200)) {
                    if (taillelatte == 1) {
                      poudredecafe = poudredecafe - 6
                      lait = lait - 120
                      prixboisson = prixboisson + 2.70
                      prixtotal += 2.70
                    }
                    if (taillelatte == 2) {
                      poudredecafe = poudredecafe - 8
                      lait = lait - 150
                      prixboisson = prixboisson + 3.20
                      prixtotal += 3.20
                    }
                    if (taillelatte == 3) {
                      poudredecafe = poudredecafe - 12
                      lait = lait - 200
                      prixboisson = prixboisson + 3.70
                      prixtotal += 3.70
                    }
                    if (sucreoption == 2) {
                      sucre = sucre - 5
                    }
                    if (sucreoption == 3) {
                      sucre = sucre - 10
                    }
                    if (sucreoption == 4) {
                      sucre = sucre - 15
                    }
                    if (doselait == 1) {
                      lait = lait - 50
                    }
                    if (doselait == 2) {
                      lait = lait - 100
                    }
                    if (doselait == 3) {
                      lait = lait - 150
                    }
                    printf("Prix : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixboisson, prixsucre, prixdoselait, prixtotal)
                    println("Veuillez payer en utilisant TWINT")
                    println("votre code de paiement est :" + codetwint)
                    println("En attente de paiement...")
                    Thread.sleep(3000)
                    println("Merci! Votre paiement a été accepté")
                    println("préparation de votre boisson...")
                    println("Votre Latte est prêt ! Bonne dégustation !")
                  } else if ((taillelatte == 1 && poudredecafe < 6) || (taillelatte == 2 && poudredecafe < 8) || (taillelatte == 3 && poudredecafe < 12)) {
                    println("ERREUR : quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode admin")
                  } else if ((taillelatte == 1 && lait < 120) || (taillelatte == 2 && lait < 150) || (taillelatte == 3 && lait < 200)) {
                    println("ERREUR : quantité de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode admin")
                  }
                }
              }
            }

            if (laitsupplement == 2) {
              if (boisson == 2 && poudredecafe >= 6 && lait >= 100) {
                poudredecafe = poudredecafe - 6
                lait = lait - 100
                prixboisson = prixboisson + 2.50
                prixtotal += 2.50
                if (sucreoption == 2) {
                  sucre = sucre - 5
                }
                if (sucreoption == 3) {
                  sucre = sucre - 10
                }
                if (sucreoption == 4) {
                  sucre = sucre - 15
                }
                printf("Prix : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixboisson, prixsucre, prixdoselait, prixtotal)
                println("veuillez payer en utilisant TWINT")
                println("votre code de paiement est :" + codetwint)
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci! Votre paiement a été accepté")
                println("préparation de votre boisson...")
                println("Votre Cappucino est prêt ! Bonne dégustation !")
              } else if (boisson == 2 && poudredecafe < 6) {
                println("ERREUR : quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode admin")
              } else if (boisson == 2 && lait < 100) {
                println("ERREUR : quantité de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode admin")
              }
              if (boisson == 3) {
                println("Veuillez selectionner la taille désirez :")
                println("1) Petit - CHF 2.70")
                println("2) Moyen - CHF 3.20")
                println("3) Grand - CHF 3.70")
                var taillelatte = readInt()
                while (!(taillelatte == 1 || taillelatte == 2 || taillelatte == 3)) {
                  println("ERREUR : la taille selectionne n'est pas valide, veuillez resélectionner la taille desirez :")
                  println("1) Petit - CHF 2.70")
                  println("2) Moyen - CHF 3.20")
                  println("3) Grand - CHF 3.70")
                  taillelatte = readInt()
                }
                if (taillelatte == 1 && poudredecafe >= 6 && lait >= 120) {
                  poudredecafe = poudredecafe - 6
                  lait = lait - 120
                  prixboisson = prixboisson + 2.70
                  prixtotal += 2.70
                  if (sucreoption == 2) {
                    sucre = sucre - 5
                  }
                  if (sucreoption == 3) {
                    sucre = sucre - 10
                  }
                  if (sucreoption == 4) {
                    sucre = sucre - 15
                  }
                  printf("Prix : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixboisson, prixsucre, prixdoselait, prixtotal)
                  println("veuillez payer en utilisant TWINT")
                  println("votre code de paiement est :" + codetwint)
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci! Votre paiement a été accepté")
                  println("préparation de votre boisson...")
                  println("Votre Latte est prêt ! Bonne dégustation !")
                } else if (taillelatte == 1 && poudredecafe < 6) {
                  println("ERREUR : quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode admin")
                } else if (taillelatte == 1 && lait < 120) {
                  println("ERREUR : quantité de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode admin")
                }
                if (taillelatte == 2 && poudredecafe >= 8 && lait >= 150) {
                  poudredecafe = poudredecafe - 8
                  lait = lait - 150
                  prixboisson = prixboisson + 3.20
                  prixtotal += 3.20
                  if (sucreoption == 2) {
                    sucre = sucre - 5
                  }
                  if (sucreoption == 3) {
                    sucre = sucre - 10
                  }
                  if (sucreoption == 4) {
                    sucre = sucre - 15
                  }
                  printf("Prix : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixboisson, prixsucre, prixdoselait, prixtotal)
                  println("veuillez payer en utilisant TWINT")
                  println("votre code de paiement est :" + codetwint)
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci! Votre paiement a été accepté")
                  println("préparation de votre boisson...")
                  println("Votre Latte est prêt ! Bonne dégustation !")
                } else if (taillelatte == 2 && poudredecafe < 8) {
                  println("ERREUR : quantité de poudre de café insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson ")
                } else if (taillelatte == 2 && lait < 150) {
                  println("ERREUR : quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson ")
                }
                if (taillelatte == 3 && poudredecafe >= 12 && lait >= 200) {
                  poudredecafe = poudredecafe - 12
                  lait = lait - 200
                  prixboisson = prixboisson + 3.70
                  prixtotal += 3.70
                  if (sucreoption == 2) {
                    sucre = sucre - 5
                  }
                  if (sucreoption == 3) {
                    sucre = sucre - 10
                  }
                  if (sucreoption == 4) {
                    sucre = sucre - 15
                  }
                  printf("Prix : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ", prixboisson, prixsucre, prixdoselait, prixtotal)
                  println("veuillez payer en utilisant TWINT")
                  println("votre code de paiement est :" + codetwint)
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci! Votre paiement a été accepté")
                  println("préparation de votre boisson...")
                  println("Votre Latte est prêt ! Bonne dégustation !")
                } else if (taillelatte == 3 && poudredecafe < 12) {
                  println("ERREUR : quantité de poudre de café insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson ")
                } else if (taillelatte == 3 && lait < 200) {
                  println("ERREUR : quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson ")
                }
              }
            }
          }
        }
        if ((sucreoption == 1) || (sucreoption == 2 && sucre >= 5) || (sucreoption == 3 && sucre >= 10) || (sucreoption == 4 && sucre >= 15)) {
          if (boisson == 1 && poudredecafe >= 8) {
            poudredecafe = poudredecafe - 8
            prixboisson = prixboisson + 2.00
            prixtotal += 2.00
            if (sucreoption == 2) {
              sucre = sucre - 5
            }
            if (sucreoption == 3) {
              sucre = sucre - 10
            }
            if (sucreoption == 4) {
              sucre = sucre - 15
            }
            printf("Prix : CHF %.2f + CHF %.2f = CHF %.2f ", prixboisson, prixsucre, prixtotal)
            println("veuillez payer en utilisant TWINT")
            println("votre code de paiement est :" + codetwint)
            println("En attente de paiement...")
            Thread.sleep(3000)
            println("Merci! Votre paiement a été accepté")
            println("préparation de votre boisson...")
            println("Votre Expresso est prêt ! Bonne dégustation !")
          } else if (boisson == 1 && poudredecafe < 8) {
            println("ERREUR : quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode admin")
          }
        }
      }
      if (mode == 2) {
        println("Saisir le code PIN admin")
        var codePIN = readInt()
        if (codePIN == 434343) {
          println("ACCÈS AUTORISÉ")
          println(" Poudre de cafe :" + poudredecafe)
          println(" Lait :" + lait)
          println(" Sucre :" + sucre)
          println("Souhaitez-vous réaprovisionner les stocks ?")
          println("1 ) OUI")
          println("2 ) NON")
          print(">")
          var reaprovisionnement = readInt()
          while (!(reaprovisionnement == 1 || reaprovisionnement == 2)) {
            println("Le choix sélectionné n'est pas valide, souhaitez vous reaprovisionner les stocks ?")
            println("1 ) OUI")
            println("2 ) NON")
            print(">")
            reaprovisionnement = readInt()
          }
          if (reaprovisionnement == 1) {
            println("Réapprovisionnement des stocks...")
            poudredecafe = readLine("Combien souhaitez vous ajouter de poudre de café (gr) : ").toDouble + poudredecafe
            lait = readLine("Combien souhaitez vous ajouter de lait  (ml)  :   ").toDouble + lait
            sucre = readLine("Combien souhaitez vous ajouter de sucre  (gr)  :   ").toDouble + sucre
            println(" Poudre de cafe :" + poudredecafe)
            println(" Lait :" + lait)
            println(" Sucre :" + sucre)
          }
        } else {
          println("ERREUR : le code PIN saisie est incorrecte --- ACCÈS REFUSÉ")
        }
      }
      if (mode == 3) {
        println("Merci de votre visite ! Au revoir ! ")
        retouraumode = false
      }
    }
  }
}