import scala.io.StdIn._
import math._


object Main {
  def main(args: Array[String]): Unit = {
    var stockpoudre = 50
    var stocklait = 0.5
    var stocksucre = 30
    val codepin = 434343
    var prix1 = 0.00
    var prix2 = 0.00
    var prix3 = 0.00
    var boissonfinale = ""
    var choix = 0
    var paiementvalide = true


    while (choix != 3) {
      println("Nospresso Café ")
      println("Veuillez sélectionner votre mode : ")
      println("1) Client ")
      println("2) Admin ")
      println("3) Quitter ")
      print("> ")
      choix = readInt()
      while ((choix < 1) || (choix > 3)) {
        println("Veuillez sélectionner soit 1 pour client, soit 2 pour admin, soit 3 pour quitter. ")
        choix = readInt()
      }
      var choixvalide = false


      if (choix == 1) {
        var boisson = 0


        while (!choixvalide) {
          println("Veuillez sélectionner votre boisson : ")
          println("1) Expresso - CHF 2.00 ")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          print("> ")
          boisson = readInt()


          if (boisson == 1) {
            boissonfinale = "Expresso "
            if (stockpoudre < 8) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              paiementvalide = false


            } else {
              stockpoudre -= 8
              //println("stockpoudre: " + stockpoudre)
              prix1 = 2.00
              //println("prix: " + prix1)
              println("Boisson sélectionnée : " + boissonfinale)
              choixvalide = true
              paiementvalide = true
            }


          } else if (boisson == 2) {
            boissonfinale = "Cappuccino "
            if (stockpoudre < 6) {
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              paiementvalide = false
            }
            else if (stocklait < 0.1) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              paiementvalide = false
            }
            else if ((stockpoudre >= 6) && (stocklait >= 0.1)) {
              stockpoudre -= 6
              stocklait -= 0.1
              //println("stockpoudre: " + stockpoudre + " stocklait: " + stocklait)
              prix1 = 2.50
              //println("prix: " + prix1)
              println("Boisson sélectionnée : " + boissonfinale)
              choixvalide = true
              paiementvalide = true


            }
          }
          else if (boisson == 3) {
            {
              boissonfinale = "Latte "
              println("Veuillez selectionner la taille de votre latte : ")
              println("1) petit (2.70) ")
              println("2) moyen (3.20) ")
              println("3) grand (3.70) ")
              print("> ")
              var taillelatte = readInt()
              println(taillelatte)



              if (taillelatte == 1) {
                if (stockpoudre < 6) {
                  println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  paiementvalide = false
                }
                else if (stocklait < 0.12) {
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  paiementvalide = false
                }
                else if ((stockpoudre >= 6) && (stocklait >= 0.12)) {
                  stockpoudre -= 6
                  stocklait -= 0.12
                  //println("stockpoudre: " + stockpoudre + " stocklait: " + stocklait)
                  prix1 = 2.70
                  //println("prix: " + prix1)
                  println("Boisson sélectionnée : " + boissonfinale + " (petit) ")
                  choixvalide = true
                  paiementvalide = true
                }
              }
              else if (taillelatte == 2) {
                if (stockpoudre < 8) {
                  println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  paiementvalide = false
                }
                else if (stocklait < 0.15) {
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  paiementvalide = false
                } else if ((stockpoudre >= 8) && (stocklait >= 0.15)) {
                  stockpoudre -= 8
                  stocklait -= 0.15
                 // println("stockpoudre: " + stockpoudre + " stocklait: " + stocklait)
                  prix1 = 3.20
                  //println("prix: " + prix1)
                  println("Boisson sélectionnée : " + boissonfinale + " (moyen) ")
                  choixvalide = true
                  paiementvalide = true
                }
              } else if (taillelatte == 3) {
                if (stockpoudre < 12) {
                  println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. ")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  paiementvalide = false
                }
                else if (stocklait < 0.2) {
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  paiementvalide = false
                } else if ((stockpoudre >= 12) && (stocklait >= 0.2)) {
                  stockpoudre -= 12
                  stocklait -= 0.2
                  //println("stockpoudre: " + stockpoudre + " stocklait: " + stocklait)
                  prix1 = 3.70
                  //println("prix: " + prix1)
                  println("Boisson sélectionnée : " + boissonfinale + " (grand) ")
                  choixvalide = true
                  paiementvalide = true
                }
              }
            }
          }
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          print("> ")
          var sucre = readInt()
          if (sucre == 1) {
            stocksucre -= 0
            //println("stocksucre: " + stocksucre)
            println("Niveau de sucre : Sans sucre ")
            choixvalide = true
            paiementvalide = true
          }
          if (sucre == 2) {
            stocksucre -= 5
            //println("stocksucre: " + stocksucre)
            prix2 = 0.10
            //println("prix: " + prix2)
            println("Niveau de sucre : Peu (5g) ")
            choixvalide = true
            paiementvalide = true
          } else if (sucre == 3) {
            stocksucre -= 10
            //println("stocksucre: " + stocksucre)
            prix2 = 0.20
            //println("prix: " + prix2)
            println("Niveau de sucre : Moyen (10g) ")
            choixvalide = true
            paiementvalide = true
          } else if (sucre == 4) {
            stocksucre -= 15
            //println("stocksucre: " + stocksucre)
            prix2 = 0.30
            //println("prix: " + prix2)
            println("Niveau de sucre : Beaucoup (15g) ")
            choixvalide = true
            paiementvalide = true
          }

            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("Disponible uniquement pour Cappuccino et Latte")
            println("1) Oui")
            println("2) Non")
            var supplement = readInt()

            if (supplement == 1) {
              if (boisson == 1) {
                println("Erreur")
                println
              }else {
                println("Combien de doses ? Vous pouvez séléctionner trois au maximum (50ml)")
                var doses = readInt()

                if ((doses < 1) || (doses > 3)) {
                  println("Erreur : veuillez choisir soit 1 pour oui, soit 2 pour non.")
                  println
                }
                else if (doses == 1) {


                  if (stocklait < 0.05) {
                    println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                    println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    paiementvalide = false
                  }
                  else {
                    stocklait -= 0.05
                    //println(" stocklait: " + stocklait)
                    prix3 = doses * 0.05
                    //println("prix: " + prix3)
                    println("Lait en supplément : 1 dose (50ml) ")
                    choixvalide = true
                    paiementvalide = true


                  }
                } else if (doses == 2) {


                  if (stocklait < 0.1) {
                    println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                    println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    paiementvalide = false
                  } else {
                    stocklait -= 0.1
                    //println(" stocklait: " + stocklait)
                    prix3 = doses * 0.05
                    //println("prix: " + prix3)
                    println("Lait en supplément : 2 dose (100ml) ")
                    choixvalide = true
                    paiementvalide = true
                  }
                } else if (doses == 3) {


                  if (stocklait < 0.15) {
                    println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. ")
                    println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    paiementvalide = false
                  }
                  else {
                    stocklait -= 0.15
                    //println(" stocklait: " + stocklait)
                    prix3 = doses * 0.05
                    //println("prix: " + prix3)
                    println("Lait en supplément : 3 dose (150ml) ")
                    choixvalide = true
                    paiementvalide = true
                  }


                }
              }
            } else if (supplement == 2) {
              println("Lait en supplément : Non ")
              choixvalide = true
              paiementvalide = true
            }




if(paiementvalide){var prixtotal = prix1 + prix2 + prix3
  printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f",prix1,prix2,prix3,prixtotal)
  println
  println("Veuillez payer en utilisant Twint. ")
  val alphanum = "ABCDEFGHIJKLMNOPKRSTUVWXYZ0123456789"
  var code = ""
  for (i <- 0 to 4) {
    code = code + alphanum((math.random() * alphanum.length).toInt)
  }
  println("Votre code de paiement est : " + code)
  println(("En attente de paiement..."))
  Thread.sleep(5000)
  println("Paiement confirmé. ")
  println("Préparation de votre boisson...")
  println("Votre " + boissonfinale + "est prêt ! Bonne dégustation !")
  println
}else {
  println("Impossible de procéder au paiment, stock insuffisant.")
  println
}


      }} else if (choix == 2) {
        println("mode admin ")
        println("Entrez le code pin : ")
        if (readInt() != codepin) {
          println("Code erroné")
        }
        else {
          println(" Accès autorisé.")
          println("Stocks: ")
          println("poudre de café: " + stockpoudre + "g")
          println("Lait: " + stocklait + "L")
          println("Sucre: " + stocksucre + "g")
          println("quelle quantitée de café avez vous ajoutée (en grammes) ?")
          var reapprovisionnementcafe = readInt()
          stockpoudre += reapprovisionnementcafe
          println("quelle quantitée de lait avez vous ajoutée (en litre) ?")
          var reapprovisionnementlait = readInt()
          stocklait += reapprovisionnementlait
          println("quelle quantitée de sucre avez vous ajoutée (en grammes) ?")
          var reapprovisionnementsucre = readInt()
          stocksucre += reapprovisionnementsucre
          println("Réapprovisionnement des stocks... ")
          println("Ajout : ")
          println("poudre de café: " + stockpoudre + "g")
          println("Lait: " + stocklait + "L")
          println("Sucre: " + stocksucre + "g")
          println("Niveaux de stock mis à jour.")
          println("Retour au menu principal...")
          println
        }


      }
      else if (choix == 3) {
        println("exit")
      }
    }
    }
  }




