import io.StdIn._
object Main {
  def main(args: Array[String]): Unit = {
    import io.StdIn._
    import scala.util.Random

    var mode = 0
    var STcafe = 50
    var STsucre = 30
    var STlait = 0.5
    var boucle = 1
    while(boucle == 1){

      // Lancement
      do{
        var p1 = Random.alphanumeric(1).toString
        var p2 = Random.alphanumeric(1).toString
        var p3 = Random.alphanumeric(1).toString
        var p4 = Random.alphanumeric(1).toString
        var p5 = Random.alphanumeric(1).toString
        var paiement = p1 + p2 + p3 +p4 + p5

        mode = readLine("        Nosepresso Café  \n" + "Veuillez sélectionner votre mode :  \n" + "1) Client  \n" + "2) Admin  \n" + "3) Quittez" + "\n>").toInt
        if (mode == 1) {
          var erreur = 1
          while( erreur == 1){
            var boisson = 0
            var sucre = 0
            var lait = 0
            var dose = 0
            var latte = 0
            do{
              boisson = readLine("        Veuillez sélectionner votre boisson  \n" + "1) Expresso - CHF 2.00  \n" + "2) Cappuccino - CHF 2.50 \n" + "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)" + "\n>").toInt
            } while (!(boisson == 1 || boisson == 2 || boisson == 3))
            if( boisson == 3){
              do{
                latte = readLine("1) Petit \n" + "2) Moyen \n" + "3) Grand" + "\n>").toInt
              } while (!(latte == 1 || latte == 2 || latte == 3))
            }
            do{
              sucre = readLine("Souhaitez-vous ajouter du sucre ?  \n" + "1) Sans sucre \n" + "2) Peu (5g) - CHF 0.10  \n" + "3) Moyen (10g) - CHF 0.20  \n" + "4) Beaucoup (15g) - CHF 0.30" + "\n>").toInt
            } while (!(sucre == 1 || sucre == 2 || sucre == 3 || sucre == 4))
            if ((boisson == 2) || (boisson == 3)){
              do{
                lait = readLine("Souhaitez-vous ajouter du lait en supplément ? \n" + "1) Oui \n" + "2) Non" + "\n>").toInt
                if(lait == 1){
                  do{
                    dose = readLine("Combien de dose ? (Dose - CHF 0.05, maximum 3 doses)" + "\n>").toInt
                  } while(!(dose == 1 || dose == 2 || dose == 3))
                }
              } while(!(lait == 1 || lait == 2))
            }

            val prixE = 2.00
            val prixC = 2.50
            val prixLP = 2.70
            val prixLM = 3.20
            val prixLG = 3.70
            val sucreP = 0.10
            val sucreM = 0.20
            val sucreB = 0.30
            val prixLait = 0.05
            var prixfinal = 0.0

            // Expresso ( valable )
            if((boisson == 1) && (sucre == 1) && (STcafe >= 8)){
              var prixfinal = prixE
              printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Sans sucre \n" + "Prix total : CHF %.2f \n" + " \n" , prixfinal )
              println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a été accepté")
              println("Préparation de votre boisson... \n" + "Votre Expresso est prêt ! Bonne dégustation !")
              STcafe -= 8
              erreur = 0
            }
              else if((boisson == 1) && (sucre == 2) && (STcafe >= 8) && (STsucre >= 5)){
                var prixfinal = prixE + sucreP
                printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Peu (5g) \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixE, sucreP, prixfinal)
                println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a été accepté")
                println("Préparation de votre boisson... \n" + "Votre Expresso est prêt ! Bonne dégustation !")
                STcafe -= 8
                STsucre -= 5
                erreur = 0
              }
                else if((boisson == 1) && (sucre == 3) && (STcafe >= 8) && (STsucre >= 10)){
                  var prixfinal = prixE + sucreM
                  printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Moyen (10g) \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixE, sucreM, prixfinal)
                  println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a été accepté")
                  println("Préparation de votre boisson... \n" + "Votre Expresso est prêt ! Bonne dégustation !")
                  STcafe -= 8
                  STsucre -= 10
                  erreur = 0
                }
                  else if((boisson == 1) && (sucre == 4) && (STcafe >= 8) && (STsucre >= 15)){
                    var prixfinal = prixE + sucreB
                    printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixE, sucreB, prixfinal)
                    println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                    Thread.sleep(3000)
                    println("Merci ! Votre paiement a été accepté")
                    println("Préparation de votre boisson... \n" + "Votre Expresso est prêt ! Bonne dégustation !")
                    STcafe -= 8
                    STsucre -= 15
                    erreur = 0
                  }

            // Expresso ( avec erreur de cafe )
            if((boisson == 1) && (sucre == 1) && (STcafe < 8) && (erreur == 1)){
              printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Sans sucre \n")
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 1) && (sucre == 2) && (STcafe < 8) && (erreur == 1)){
                printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Peu (5g) \n")
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 1) && (sucre == 3) && (STcafe < 8) && (erreur == 1)){
                  printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Moyen (10g) \n")
                  println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if((boisson == 1) && (sucre == 4) && STcafe < 8 && (erreur == 1)){
                    printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Beaucoup (15g) \n")
                    println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }

            // Expresso ( avec erreur de sucre )
            if((boisson == 1) && (sucre == 2) && (STsucre < 5) && (erreur == 1)){
              printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Peu (5g) \n")
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 1) && (sucre == 3) && (STsucre < 10) && (erreur == 1)){
                printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Moyen (10g) \n")
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 1) && (sucre == 4) && (STsucre < 15) && (erreur == 1)){
                  printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Beaucoup (15g) \n")
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }

            // Cappuccino ( valable)
            if((boisson == 2) && (sucre == 1) && (lait == 2) && (STcafe >= 6) && (STlait >= 0.1)){
              var prixfinal = prixC
              printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f \n" + " \n" , prixfinal)
              println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a été accepté")
              println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
              STcafe -= 6
              STlait -= 0.1
              erreur = 0
            }
              else if((boisson == 2) && (sucre == 2) && (lait == 2) && (STcafe >= 6) && (STsucre >= 5) && (STlait >= 0.1)){
                var prixfinal = prixC + sucreP
                printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixC, sucreP, prixfinal)
                println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a été accepté")
                println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
                STcafe -= 6
                STsucre -= 5
                STlait -= 0.1
                erreur = 0
              }
                else if((boisson == 2) && (sucre == 3) && (lait == 2) && (STcafe >= 6) && (STsucre >= 10) && (STlait >= 0.1)){
                  var prixfinal = prixC + sucreM
                  printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + "\n" , prixC, sucreM, prixfinal)
                  println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a été accepté")
                  println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
                  STcafe -= 6
                  STsucre -= 10
                  STlait -= 0.1
                  erreur = 0
                }
                  else if((boisson == 2) && (sucre == 4) && (lait == 2) && (STcafe >= 6) && (STsucre >= 15) && (STlait >= 0.1)){
                    var prixfinal = prixC + sucreB
                    printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixC, sucreB, prixfinal)
                    println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                    Thread.sleep(3000)
                    println("Merci ! Votre paiement a été accepté")
                    println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
                    STcafe -= 6
                    STsucre -= 15
                    STlait -= 0.1
                    erreur = 0
                  }
                    else if((boisson == 2) && (sucre == 1) && (lait == 1) && (STcafe >= 6) && (STlait >= 0.1 + (0.05 * dose))){
                      var prixfinal = prixC + (prixLait * dose)
                      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n", prixC, (prixLait * dose), prixfinal)
                      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                      Thread.sleep(3000)
                      println("Merci ! Votre paiement a été accepté")
                      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
                      STcafe -= 6
                      STlait -= 0.1 + ( 0.05 * dose)
                      erreur = 0
                    }
                      else if ((boisson == 2) && (sucre == 2) && (lait == 1) && (STcafe >= 6) && (STsucre >= 5) && (STlait >= 0.1 + (0.05 * dose))){
                        var prixfinal = prixC + sucreP + (prixLait * dose)
                        printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixC, sucreP, (prixLait * dose), prixfinal)
                        println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                        Thread.sleep(3000)
                        println("Merci ! Votre paiement a été accepté")
                        println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
                        STcafe -= 6
                        STsucre -= 5
                        STlait -= 0.1 + ( 0.05 * dose)
                        erreur = 0
                      }
                        else if((boisson == 2) && (sucre == 3) && (lait == 1) && (STcafe >= 6) && (STsucre >= 10) && (STlait >= 0.1 + (0.05 * dose))){
                          var prixfinal = prixC + sucreM + (prixLait * dose)
                          printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixC, sucreM, (prixLait * dose), prixfinal)
                          println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                          Thread.sleep(3000)
                          println("Merci ! Votre paiement a été accepté")
                          println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
                          STcafe -= 6
                          STsucre -= 10
                          STlait -= 0.1 + ( 0.05 * dose)
                          erreur = 0
                        }
                          else if((boisson == 2) && (sucre == 4) && (lait == 1) && (STcafe >= 6) && (STsucre >= 15) && (STlait >= 0.1 + (0.05 * dose))){
                            var prixfinal = prixC + sucreB + (prixLait * dose)
                            printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixC, sucreB, (prixLait * dose), prixfinal)
                            println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                            Thread.sleep(3000)
                            println("Merci ! Votre paiement a été accepté")
                            println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
                            STcafe -= 6
                            STsucre -= 15
                            STlait -= 0.1 + ( 0.05 * dose)
                            erreur = 0
                          }

            // Cappuccino ( avec erreur de cafe)
            if((boisson == 2) && (sucre == 1) && (lait == 2) && (STcafe < 6) && (erreur == 1)){
              printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 2) && (sucre == 2) && (lait == 2) && (STcafe < 6) && (erreur == 1)){
                printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 2) && (sucre == 3) && (lait == 2) && (STcafe < 6) && (erreur == 1)){
                  printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
                  println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if((boisson == 2) && (sucre == 4) && (lait == 2) && (STcafe < 6) && (erreur == 1)){
                    printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
                    println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                    else if((boisson == 2) && (sucre == 1) && (lait == 1) && (STcafe < 6) && (erreur == 1)){
                      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    }
                      else if ((boisson == 2) && (sucre == 2) && (lait == 1) && (STcafe < 6) && (erreur == 1)){
                        printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }
                        else if((boisson == 2) && (sucre == 3) && (lait == 1) && (STcafe < 6) && (erreur == 1)){
                          printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                        }
                          else if((boisson == 2) && (sucre == 4) && (lait == 1) && (STcafe < 6) && (erreur == 1)){
                            printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                          }

            // Cappuccino ( avec erreur de sucre)
            if((boisson == 2) && (sucre == 2) && (lait == 2) && (STsucre < 5) && (erreur == 1)){
              printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 2) && (sucre == 3) && (lait == 2) && (STsucre < 10) && (erreur == 1)){
                printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 2) && (sucre == 4) && (lait == 2) && (STsucre < 15) && (erreur == 1)){
                  printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if ((boisson == 2) && (sucre == 2) && (lait == 1) && (STsucre < 5) && (erreur == 1)){
                    printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                    else if((boisson == 2) && (sucre == 3) && (lait == 1) && (STsucre < 10) && (erreur == 1)){
                      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    }
                      else if((boisson == 2) && (sucre == 4) && (lait == 1) && (STsucre < 15) && (erreur == 1)){
                        printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }

            // Cappuccino ( avec erreur de lait)
            if((boisson == 2) && (sucre == 1) && (lait == 2) && (STlait < 0.1) && (erreur == 1)){
              printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 2) && (sucre == 2) && (lait == 2) && (STlait < 0.1) && (erreur == 1)){
                printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 2) && (sucre == 3) && (lait == 2) && (STlait < 0.1) && (erreur == 1)){
                  printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if((boisson == 2) && (sucre == 4) && (lait == 2) && (STlait < 0.1) && (erreur == 1)){
                    printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
                    println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                    else if((boisson == 2) && (sucre == 1) && (lait == 1) && (STlait < 0.1 + (0.05 * dose)) && (erreur == 1)){
                      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    }
                      else if ((boisson == 2) && (sucre == 2) && (lait == 1) && (STlait < 0.1 + (0.05 * dose)) && (erreur == 1)){
                        printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }
                        else if((boisson == 2) && (sucre == 3) && (lait == 1) && (STlait < 0.1 + (0.05 * dose)) && (erreur == 1)){
                          printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                        }
                          else if((boisson == 2) && (sucre == 4) && (lait == 1) && (STlait < 0.1 + (0.05 * dose)) && (erreur == 1)){
                            printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                          }

            // Latte Petit ( valable)
            if((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (STcafe >= 6) && (STlait >= 0.12)){
              var prixfinal = prixLP
              printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f \n" + " \n" , prixfinal)
              println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a été accepté")
              println("Préparation de votre boisson... \n" + "Votre Latte est prêt ! Bonne dégustation !")
              STcafe -= 6
              STlait -= 0.12
              erreur = 0
            }
              else if((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (STcafe >= 6) && (STsucre >= 5) && (STlait >= 0.12)){
                var prixfinal = prixLP + sucreP
                printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLP, sucreP, prixfinal)
                println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a été accepté")
                println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
                STcafe -= 6
                STsucre -= 5
                STlait -= 0.12
                erreur = 0
              }
                else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (STcafe >= 6) && (STsucre >= 10) && (STlait >= 0.12)){
                  var prixfinal = prixLP + sucreM
                  printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLP, sucreM, prixfinal)
                  println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a été accepté")
                  println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
                  STcafe -= 6
                  STsucre -= 10
                  STlait -= 0.12
                  erreur = 0
                }
                  else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (STcafe >= 6) && (STsucre >= 15) && (STlait >= 0.12)){
                    var prixfinal = prixLP + sucreB
                    printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLP, sucreB, prixfinal)
                    println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                    Thread.sleep(3000)
                    println("Merci ! Votre paiement a été accepté")
                    println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
                    STcafe -= 6
                    STsucre -= 15
                    STlait -= 0.12
                    erreur = 0
                  }
                    else if((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 1) && (STcafe >= 6) && (STlait >= 0.12 + (0.05 * dose))){
                      var prixfinal = prixLP + (prixLait * dose)
                      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n", prixLP, (prixLait * dose), prixfinal)
                      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                      Thread.sleep(3000)
                      println("Merci ! Votre paiement a été accepté")
                      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
                      STcafe -= 6
                      STlait -= 0.12 + ( 0.05 * dose)
                      erreur = 0
                    }
                      else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (STcafe >= 6) && (STsucre >= 5) && (STlait >= 0.12 + (0.05 * dose))){
                        var prixfinal = prixLP + sucreP + (prixLait * dose)
                        printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLP, sucreP, (prixLait * dose), prixfinal)
                        println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                        Thread.sleep(3000)
                        println("Merci ! Votre paiement a été accepté")
                        println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
                        STcafe -= 6
                        STsucre -= 5
                        STlait -= 0.12 + ( 0.05 * dose)
                        erreur = 0
                      }
                        else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (STcafe >= 6) && (STsucre >= 10) && (STlait >= 0.12 + (0.05 * dose))){
                          var prixfinal = prixLP + sucreM + (prixLait * dose)
                          printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLP, sucreM, (prixLait * dose), prixfinal)
                          println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                          Thread.sleep(3000)
                          println("Merci ! Votre paiement a été accepté")
                          println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
                          STcafe -= 6
                          STsucre -= 10
                          STlait -= 0.12 + ( 0.05 * dose)
                          erreur = 0
                        }
                          else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (STcafe >= 6) && (STsucre >= 15) && (STlait >= 0.12 + (0.05 * dose))){
                            var prixfinal = prixLP + sucreB + (prixLait * dose)
                            printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLP, sucreB, (prixLait * dose), prixfinal)
                            println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                            Thread.sleep(3000)
                            println("Merci ! Votre paiement a été accepté")
                            println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
                            STcafe -= 6
                            STsucre -= 15
                            STlait -= 0.12 + ( 0.05 * dose)
                            erreur = 0
                          }

            // latte petit ( avec probleme de cafe)
            if((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (STcafe < 6) && (erreur == 1)){
              printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (STcafe < 6) && (erreur == 1)){
                printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (STcafe < 6) && (erreur == 1)){
                  printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
                  println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (STcafe < 6) && (erreur == 1)){
                    printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
                    println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                    else if((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 1) && (STcafe < 6) && (erreur == 1)){
                      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    }
                      else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (STcafe < 6) && (erreur == 1)){
                        printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }
                        else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (STcafe < 6) && (erreur == 1)){
                          printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                        }
                          else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (STcafe < 6) && (erreur == 1)){
                            printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                          }

            // Latte petit ( avec erreur de sucre)
            if((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (STsucre < 5) && (erreur == 1)){
              printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (STsucre < 10) && (erreur == 1)){
                printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (STsucre < 15) && (erreur == 1)){
                  printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (STsucre < 5) && (erreur == 1)){
                    printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                    else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (STsucre < 10) && (erreur == 1)){
                      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    }
                      else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (STsucre < 15) && (erreur == 1)){
                        printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }

            // Latte Petit ( avec erreur de lait)
            if((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (STlait < 0.12) && (erreur == 1)){
              printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (STlait < 0.12) && (erreur == 1)){
                printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (STlait < 0.12) && (erreur == 1)){
                  printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (STlait < 0.12) && (erreur == 1)){
                    printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
                    println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                    else if((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 1) && (STlait < 0.12 + (0.05 * dose)) && (erreur == 1)){
                      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    }
                      else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (STlait < 0.12 + (0.05 * dose)) && (erreur == 1)){
                        printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }
                        else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (STlait < 0.12 + (0.05 * dose)) && (erreur == 1)){
                          printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                        }
                          else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (STlait < 0.12 + (0.05 * dose)) && (erreur == 1)){
                            printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                          }

            // Latte Moyen ( valable )
            if((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 2) && (STcafe >= 8) && (STlait >= 0.15)){
              var prixfinal = prixLM
              printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f \n" + " \n" , prixfinal)
              println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a été accepté")
              println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
              STcafe -= 8
              STlait -= 0.15
              erreur = 0
            }
              else if((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (STcafe >= 8) && (STsucre >= 5) && (STlait >= 0.15)){
                var prixfinal = prixLM + sucreP
                printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLM, sucreP, prixfinal)
                println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a été accepté")
                println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
                STcafe -= 8
                STsucre -= 5
                STlait -= 0.15
                erreur = 0
              }
                else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (STcafe >= 8) && (STsucre >= 10) && (STlait >= 0.15)){
                  var prixfinal = prixLM + sucreM
                  printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLM, sucreM, prixfinal)
                  println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a été accepté")
                  println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
                  STcafe -= 8
                  STsucre -= 10
                  STlait -= 0.15
                  erreur = 0
                }
                  else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (STcafe >= 8) && (STsucre >= 15) && (STlait >= 0.15)){
                    var prixfinal = prixLM + sucreB
                    printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLM, sucreB, prixfinal)
                    println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                    Thread.sleep(3000)
                    println("Merci ! Votre paiement a été accepté")
                    println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
                    STcafe -= 8
                    STsucre -= 15
                    STlait -= 0.15
                    erreur = 0
                  }
                    else if((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 1) && (STcafe >= 8) && (STlait >= 0.15 + (0.05 * dose))){
                      var prixfinal = prixLM + (prixLait * dose)
                      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n", prixLM, (prixLait * dose), prixfinal)
                      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                      Thread.sleep(3000)
                      println("Merci ! Votre paiement a été accepté")
                      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
                      STcafe -= 8
                      STlait -= 0.15 + ( 0.05 * dose)
                      erreur = 0
                    }
                      else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (STcafe >= 8) && (STsucre >= 5) && (STlait >= 0.15 + (0.05 * dose))){
                        var prixfinal = prixLM + sucreP + (prixLait * dose)
                        printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLM, sucreP, (prixLait * dose), prixfinal)
                        println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                        Thread.sleep(3000)
                        println("Merci ! Votre paiement a été accepté")
                        println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
                        STcafe -= 8
                        STsucre -= 5
                        STlait -= 0.15 + ( 0.05 * dose)
                        erreur = 0
                      }
                        else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (STcafe >= 8) && (STsucre >= 10) && (STlait >= 0.15 + (0.05 * dose))){
                          var prixfinal = prixLM + sucreM + (prixLait * dose)
                          printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLM, sucreM, (prixLait * dose), prixfinal)
                          println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                          Thread.sleep(3000)
                          println("Merci ! Votre paiement a été accepté")
                          println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
                          STcafe -= 8
                          STsucre -= 10
                          STlait -= 0.15 + ( 0.05 * dose)
                          erreur = 0
                        }
                          else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (STcafe >= 8) && (STsucre >= 15) && (STlait >= 0.15 + (0.05 * dose))){
                            var prixfinal = prixLM + sucreB + (prixLait * dose)
                            printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLM, sucreB, (prixLait * dose), prixfinal)
                            println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                            Thread.sleep(3000)
                            println("Merci ! Votre paiement a été accepté")
                            println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
                            STcafe -= 8
                            STsucre -= 15
                            STlait -= 0.15 + ( 0.05 * dose)
                            erreur = 0
                          }

            // latte moyen ( avec probleme de cafe)
            if((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 2) && (STcafe < 8) && (erreur == 1)){
              printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (STcafe < 8) && (erreur == 1)){
                printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (STcafe < 8) && (erreur == 1)){
                  printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
                  println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (STcafe < 8) && (erreur == 1)){
                    printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
                    println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                    else if((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 1) && (STcafe < 8) && (erreur == 1)){
                      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    }
                      else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (STcafe < 8) && (erreur == 1)){
                        printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }
                        else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (STcafe < 8) && (erreur == 1)){
                          printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                        }
                          else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (STcafe < 8) && (erreur == 1)){
                            printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                          }

            // Latte moyen ( avec erreur de sucre)
            if((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (STsucre < 5) && (erreur == 1)){
              printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (STsucre < 10) && (erreur == 1)){
                printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (STsucre < 15) && (erreur == 1)){
                  printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (STsucre < 5) && (erreur == 1)){
                    printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                    else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (STsucre < 10) && (erreur == 1)){
                      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    }
                      else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (STsucre < 15) && (erreur == 1)){
                        printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }

            // Latte moyen ( avec erreur de lait)
            if((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 2) && (STlait < 0.15) && (erreur == 1)){
              printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (STlait < 0.15) && (erreur == 1)){
                printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (STlait < 0.15) && (erreur == 1)){
                  printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (STlait < 0.15) && (erreur == 1)){
                    printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
                    println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                    else if((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 1) && (STlait < 0.15 + (0.05 * dose)) && (erreur == 1)){
                      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    }
                      else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (STlait < 0.15 + (0.05 * dose)) && (erreur == 1)){
                        printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }
                        else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (STlait < 0.15 + (0.05 * dose)) && (erreur == 1)){
                          printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                        }
                          else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (STlait < 0.15 + (0.05 * dose)) && (erreur == 1)){
                            printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                          }

            // Latte Grand ( valable)
            if((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 2) && (STcafe >= 12) && (STlait >= 0.2)){
              var prixfinal = prixLG
              printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f \n" + " \n" , prixfinal)
              println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a été accepté")
              println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
              STcafe -= 12
              STlait -= 0.2
              erreur = 0
            }
              else if((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (STcafe >= 12) && (STsucre >= 5) && (STlait >= 0.2)){
                var prixfinal = prixLG + sucreP
                printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLG, sucreP, prixfinal)
                println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a été accepté")
                println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
                STcafe -= 6
                STsucre -= 5
                STlait -= 0.2
                erreur = 0
              }
                else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (STcafe >= 12) && (STsucre >= 10) && (STlait >= 0.2)){
                  var prixfinal = prixLG + sucreM
                  printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLG, sucreM, prixfinal)
                  println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a été accepté")
                  println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
                  STcafe -= 12
                  STsucre -= 10
                  STlait -= 0.2
                  erreur = 0
                }
                  else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (STcafe >= 12) && (STsucre >= 15) && (STlait >= 0.2)){
                    var prixfinal = prixLG + sucreB
                    printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLG, sucreB, prixfinal)
                    println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                    Thread.sleep(3000)
                    println("Merci ! Votre paiement a été accepté")
                    println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
                    STcafe -= 12
                    STsucre -= 15
                    STlait -= 0.2
                    erreur = 0
                  }
                    else if((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 1) && (STcafe >= 12) && (STlait >= 0.2 + (0.05 * dose))){
                      var prixfinal = prixLG + (prixLait * dose)
                      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n", prixLG, (prixLait * dose), prixfinal)
                      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                      Thread.sleep(3000)
                      println("Merci ! Votre paiement a été accepté")
                      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
                      STcafe -= 12
                      STlait -= 0.2 + ( 0.05 * dose)
                      erreur = 0
                    }
                      else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (STcafe >= 12) && (STsucre >= 5) && (STlait >= 0.2 + (0.05 * dose))){
                        var prixfinal = prixLG + sucreP + (prixLait * dose)
                        printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLG, sucreP, (prixLait * dose), prixfinal)
                        println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                        Thread.sleep(3000)
                        println("Merci ! Votre paiement a été accepté")
                        println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
                        STcafe -= 12
                        STsucre -= 5
                        STlait -= 0.2 + ( 0.05 * dose)
                        erreur = 0
                      }
                        else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (STcafe >= 12) && (STsucre >= 10) && (STlait >= 0.2 + (0.05 * dose))){
                          var prixfinal = prixLG + sucreM + (prixLait * dose)
                          printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLG, sucreM, (prixLait * dose), prixfinal)
                          println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                          Thread.sleep(3000)
                          println("Merci ! Votre paiement a été accepté")
                          println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
                          STcafe -= 12
                          STsucre -= 10
                          STlait -= 0.2 + ( 0.05 * dose)
                          erreur = 0
                        }
                          else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (STcafe >= 12) && (STsucre >= 15) && (STlait >= 0.2 + (0.05 * dose))){
                            var prixfinal = prixLG + sucreB + (prixLait * dose)
                            printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLG, sucreB, (prixLait * dose), prixfinal)
                            println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
                            Thread.sleep(3000)
                            println("Merci ! Votre paiement a été accepté")
                            println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
                            STcafe -= 12
                            STsucre -= 15
                            STlait -= 0.2 + ( 0.05 * dose)
                            erreur = 0
                          }

            // latte grand ( avec probleme de cafe)
            if((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 2) && (STcafe < 12) && (erreur == 1)){
              printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
              println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (STcafe < 12) && (erreur == 1)){
                printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (STcafe < 12) && (erreur == 1)){
                  printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
                  println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (STcafe < 12) && (erreur == 1)){
                    printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
                    println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                    else if((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 1) && (STcafe < 12) && (erreur == 1)){
                      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    }
                      else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (STcafe < 12) && (erreur == 1)){
                        printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }
                        else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (STcafe < 12) && (erreur == 1)){
                          printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                        }
                          else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (STcafe < 12) && (erreur == 1)){
                            printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                          }

            // Latte grand ( avec erreur de sucre)
            if((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (STsucre < 5) && (erreur == 1)){
              printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (STsucre < 10) && (erreur == 1)){
                printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (STsucre < 15) && (erreur == 1)){
                  printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (STsucre < 5) && (erreur == 1)){
                    printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                    else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (STsucre < 10) && (erreur == 1)){
                      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    }
                      else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (STsucre < 15) && (erreur == 1)){
                        printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }

            // Latte grand ( avec erreur de lait)
            if((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 2) && (STlait < 0.2) && (erreur == 1)){
              printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
            }
              else if((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (STlait < 0.2) && (erreur == 1)){
                printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
                else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (STlait < 0.2) && (erreur == 1)){
                  printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                }
                  else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (STlait < 0.2) && (erreur == 1)){
                    printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
                    println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  }
                    else if((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 1) && (STlait < 0.2 + (0.05 * dose)) && (erreur == 1)){
                      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                    }
                      else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (STlait < 0.2 + (0.05 * dose)) && (erreur == 1)){
                        printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                      }
                        else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (STlait < 0.2 + (0.05 * dose)) && (erreur == 1)){
                          printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                        }
                          else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (STlait < 0.2 + (0.05 * dose)) && (erreur == 1)){
                            printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
                            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                          }
          }
        }
        else if (mode == 2){
          var code = 0
          code = readLine("Mode Admin \n" + "Entrez le code PIN :" + "\n>").toInt
          if(!(code == 434343)){
            do{
              code = readLine("Mauvais PIN, veuillez réessayer :" + "\n>").toInt
            }while(!(code == 434343))}

          println("Accès autorisé. \n" + "\n" + "Stocks : \n" + "   Poudre de café : " + STcafe + " grammes"+ "\n" + "   Lait           : " + STlait + " Litre"+ "\n" + "   Sucre          : " + STsucre + " grammes \n")
          println("Réapprovisionnement des stocks... \n" + "Ajout :")
          STcafe += readLine("   Poudre de café : ").toInt
          STlait += readLine("   Lait           : ").toDouble
          STsucre += readLine("   Sucre          : ").toInt
          println("Niveaux de stock mis à jour. \n" + "Retour au menu principal...")

        }
        else if (mode == 3) {
          println("Merci ! Au revoir !")
          boucle = 0}
      } while (!(mode == 1 || mode == 2 || mode == 3))
    }
  }
}