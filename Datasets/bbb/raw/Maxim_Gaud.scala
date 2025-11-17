import io.StdIn._
import math._


      object main {
            def validatePin(machineId: Int, MachinePins: Array[String]): Boolean = {
                  var tentatives = 3
                  println("Vous avez choisi la machine "+ machineId)
                  while (tentatives > 0) {
                        println(s"Veuillez entrer le code PIN (tentatives restantes: $tentatives)")
                        var pin = readLine("> ")
                        if (pin == MachinePins(machineId)) {
                              println("Accès autorisé à la machine " + machineId)
                              return true
                        } else {
                              tentatives -= 1
                              if(tentatives > 0){
                                    println("Code PIN incorrect. Veuillez réessayer.")
                              }else {
                                    println("Accès refusé! Nombre maximal de tentatives atteint.\nFin de la cession\n")
                                    return false
                              }
                        }
                  }
                  return false
            }

            def updatePin(machineId: Int, MachinePins: Array[String]): Unit = {
                  println("Mise à jour du code PIN pour la machine " + machineId + "\nVeuillez entrer un nouveau code a 6 chiffres")
                  var nouveaupin = readLine("> ")
                  while (nouveaupin.length != 6) {
                        println("Veuillez entrer un code valide ! ")
                        nouveaupin = readLine("> ")
                  }
                  MachinePins(machineId) = nouveaupin
                  println("Nouveau code PIN mis à jour avec succès!\nRetour au menu principal...\n ")
            }

            def serveClient(machineId: Int, coffeeStocks: Array[Int],
                            sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {

                  var codepin = Array.fill(10)("434343")
                  var Code = "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  var TWINT = ""
                  for (i <- 1 to 5) {
                        var index = (random() * 36).toInt
                        TWINT += Code(index)
                  }

                  //var StockCafe =
                  coffeeStocks(machineId)
                  //var StockSucre =
                  sugarStocks(machineId)
                  //var StockLait =
                  milkStocks(machineId)
                  var TailleLatte = 0
                  var choix = 0
                  var Cafe = 0
                  var Lait = 0
                  var Prix = 0.0
                  var prixboisson = 0.0
                  var prixsucre = 0.1
                  var prixlait = 0.05
                  var Dose = 0
                  var SucrAffichage = ""
                  var LaitAffichage = ""

                  println("\nMachine "+machineId+"\nVous êtes dans le mode client. Veuillez choisir votre boisson\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Large)")
                  var ChoixCafe = readLine("> ").toInt
                  while ((ChoixCafe != 1) && (ChoixCafe != 2) && (ChoixCafe != 3)) {
                        println("\nVeuillez choisir parmis les propositions ci-dessous.\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Large)")
                        ChoixCafe = readLine("> ").toInt
                  }
                  var boisson = ""
                  if (ChoixCafe == 1) {
                        coffeeStocks(machineId) -= 8
                        Prix = 2.0
                        prixboisson = 2.0
                        boisson = "Expresso"
                  }
                  if (ChoixCafe == 2) {
                        coffeeStocks(machineId) -= 6
                        milkStocks(machineId) -= 100
                        Prix = 2.5
                        prixboisson = 2.5
                        boisson = "Cappuccino"
                  }
                  if (ChoixCafe == 3) {
                        while ((TailleLatte != 1) && (TailleLatte != 2) && (TailleLatte != 3)) {
                              println("Veuillez sélectionner la taile de votre Latte:\n1) Petit (CHF 2.70) \n2) Moyen (CHF 3.20) \n3) Large (CHF 3.70) ")
                              TailleLatte = readLine("> ").toInt
                              if (TailleLatte == 1) {
                                    coffeeStocks(machineId) -= 6
                                    milkStocks(machineId) -= 100
                                    Prix = 2.7
                                    prixboisson = 2.7
                                    boisson = "Latte petit"
                              } else if (TailleLatte == 2) {
                                    coffeeStocks(machineId) -= 8
                                    milkStocks(machineId) -= 120
                                    Prix = 3.2
                                    prixboisson = 3.2
                                    boisson = "Latte moyen"
                              } else if (TailleLatte == 3) {
                                    coffeeStocks(machineId) -= 12
                                    milkStocks(machineId) -= 150
                                    Prix = 3.7
                                    prixboisson = 3.7
                                    boisson = "Latte large"
                              } else {
                                    println("Veuillez entrer une valeur valide! ")
                                    TailleLatte = readLine("> ").toInt
                              }
                        }
                  }
                  //SUCRE SUPLéMENTAIRE
                  var SucreSupp = -1
                  while ((SucreSupp != 0) && (SucreSupp != 1) && (SucreSupp != 2) && (SucreSupp != 3)) {
                        println("Voulez vous ajouter du  sucre ?\n0) Sans sucre\n1) Peu (5g) - CHF 0.10\n2) Moyen (10g) - CHF 0.20\n3) Beaucoup (15g) - CHF 0.30")
                        SucreSupp = readLine("> ").toInt
                        if ((SucreSupp != 0) && (SucreSupp != 1) && (SucreSupp != 2) && (SucreSupp != 3)) {
                              println("Choisissez un montant valide! ")
                              SucreSupp = readLine("> ").toInt
                        }
                  }
                  if (SucreSupp == 0) {
                        SucrAffichage = "Sans sucre"
                        prixsucre = 0.0
                  } else if (SucreSupp == 1) {
                        SucrAffichage = "Peu de sucre (5g)"
                        Prix += 0.1
                        prixsucre = 0.1
                        sugarStocks(machineId) -= 5
                  } else if (SucreSupp == 2) {
                        SucrAffichage = "Sucre moyen (10g)"
                        Prix += 0.2
                        prixsucre = 0.2
                        sugarStocks(machineId) -= 10
                  } else if (SucreSupp == 3) {
                        SucrAffichage = "Beaucoup de sucre (15g)"
                        Prix += 0.3
                        prixsucre = 0.3
                        sugarStocks(machineId) -= 15
                  }
                  //LAIT SUPPLéMENTAIRE
                  Dose = 0
                  var LaitSupp = 0
                  if ((ChoixCafe == 2) || (ChoixCafe == 3)) {
                        while ((LaitSupp != 1) && (LaitSupp != 2)) {
                              println("Voulez vous du lait supplémentaire ?\n1) Oui\n2) Non")
                              LaitSupp = readLine("> ").toInt
                              if ((LaitSupp != 1) && (LaitSupp != 2)) {
                                    println("Veuillez entrer une valeur valide! ")
                                    LaitSupp = readLine("> ").toInt
                              }
                        }
                        if (LaitSupp == 1) {
                              while (Dose < 1 || Dose > 3) {
                                    println("Combien de doses souhaitez-vous ? (vous avez le droit à 3 doses max)")
                                    Dose = readLine("> ").toInt
                                    if (Dose < 1 || Dose > 3) {
                                          println("Veuillez entrer une valeur entre 1 et 3! ")
                                          Dose = readLine("> ").toInt
                                    }
                              }
                              if (Dose == 1) {
                                    Prix += 0.05
                                    prixlait = 0.05
                                    LaitAffichage = " 1 dose "
                                    milkStocks(machineId) -= 50
                              } else if (Dose == 2) {
                                    Prix += 0.1
                                    prixlait = 0.1
                                    LaitAffichage = " 2 doses "
                                    milkStocks(machineId) -= 100
                              } else {
                                    Prix += 0.15
                                    prixlait = 0.15
                                    LaitAffichage = " 3 doses "
                                    milkStocks(machineId) -= 150
                              }
                        } else if(LaitSupp == 2){
                              LaitAffichage = " 0 dose "
                              prixlait = 0.0
                        }
                  } else if ((ChoixCafe == 1) || (LaitSupp == 2)) {
                        LaitAffichage = " 0 dose "
                        prixlait = 0.0
                  }
                  if ((coffeeStocks(machineId) < Cafe) || (sugarStocks(machineId) < (SucreSupp * 5)) || (milkStocks(machineId) < (Dose * 50))) {
                        if (coffeeStocks(machineId) < Cafe) {
                              println("Erreur: il n'y a pas assez de poudre à café pour votre boisson")
                        } else if (sugarStocks(machineId) < (SucreSupp * 5)) {
                              println("Erreur: il n'y a pas assez de sucre pour votre boisson")
                        } else if (milkStocks(machineId) < (Dose * 50)) {
                              println("Erreur: il n'y a pas assez de lait pour votre boisson")
                        }
                        println("Veuillez choisir une autre machine ou verifier les stocks dans admin\n")
                        //le choix de la machine viendra après la demande du choix de mode
                        return false
                  }

                  println("Votre commande: " + boisson + " - " + prixboisson + " CHF, avec " + SucrAffichage + " - " + prixsucre + " CHF et" + LaitAffichage + "de lait" + " - " + prixlait + " CHF =")
                  printf("%.2f", Prix)
                  println("\nVeuillez payez en utilisant Twint.\nVotre code de paiement est : " + TWINT)
                  println("(En attente de validation du paiement...)")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a été accepté.\n")
                  //Apres paiement
                  println("Préparation de votre boisson...\n[...]\nVotre " + boisson + " est prêt ! Bonne dégustation !\n")
                return true
            }
                  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                                     sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
                        var StockCafe = coffeeStocks(machineId).toDouble
                        var StockSucre = sugarStocks(machineId).toDouble
                        var StockLait = milkStocks(machineId).toDouble
                        var choixrestock = 0
                        println("Voici les stocks: \n")
                        println("Poudre à cafe: " + StockCafe +" grammes")
                        println("Sucre: " + StockSucre +" grammes")
                        println("Lait: " + StockLait/1000 +" Litres")
                        println("\nVoulez vous ajouter des ingrédients ?\n1) Oui\n2) Non ")
                        choixrestock = readLine("> ").toInt
                        if(choixrestock == 1) {
                              var quantiteCafe = -1
                              while(quantiteCafe < 0) {
                                    quantiteCafe = readLine("Ajout de poudre à café:\n> grammes ").toInt
                                    if(quantiteCafe < 0){
                                          println("Veuillez mettre des valeurs positives SVP.")
                                    }
                              }
                              coffeeStocks(machineId) += quantiteCafe

                              var quantiteSucre = -1
                              while(quantiteSucre < 0) {
                                    quantiteSucre = readLine("Ajout de sucre:\n> grammes ").toInt
                                    if(quantiteSucre < 0){
                                          println("Veuillez mettre des valeurs positives SVP.")
                                    }
                              }
                              sugarStocks(machineId) += quantiteSucre

                              var quantiteLait = -1
                              while(quantiteLait < 0) {
                                    quantiteLait = readLine("Ajout de lait:\n> millilitres ").toInt
                                    if(quantiteLait < 0){
                                          println("Veuillez mettre des valeurs positives SVP.")
                                    }
                              }
                              milkStocks(machineId) += quantiteLait
                              println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...\n")
                        }else{
                              println(" ")
                        }
                  }


                  def main(args: Array[String]): Unit = {
                        val nbMachines = 5
                        val MachinePins = Array.fill(nbMachines+1)("434343") // la machine 0 est  ignorée
                        val coffeStocks = Array.fill(nbMachines+1)(50)
                        val sugarStocks = Array.fill(nbMachines+1)(30)
                        val milkStocks = Array.fill(nbMachines+1)(500)
                        var quitter = false
                        var Code = "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        var TWINT = ""
                        for (i <- 1 to 5) {
                              var index = (random() * 36).toInt
                              TWINT += Code(index)
                        }
                        var choix = 0
                        while (!quitter) {
                              println("Nospresso Cafe\n" + "Sélectionnez votre mode:\n" + "1) Client\n" + "2) Admin\n" + "3) Quitter")
                              choix = readLine("> ").toInt
                              while ((choix != 1) && (choix != 2) && (choix != 3)) {
                                    println("Veuillez entrer une valeur valide.\n")
                                    println("Nospresso Cafe\n" + "Sélectionnez votre mode:\n" + "1) Client\n" + "2) Admin\n" + "3) Quitter")
                                    choix = readLine("> ").toInt
                              }
                              if (choix == 1) {
                                    println("Veuillez choisir votre machine (1-5).")
                                    var machineId = readLine("> ").toInt
                                    while (machineId < 1 || machineId > 5) {
                                          println("Veuillez choisir une machine entre 1 et 5 SVP.")
                                          machineId = readLine("> ").toInt
                                    }
                                    serveClient(machineId, coffeStocks, sugarStocks, milkStocks)
                              }
                              if (choix == 2) {
                                    println("Veuillez choisir votre machine (1-5).")
                                    var machineId = readLine("> ").toInt
                                    while ((machineId < 1) || (machineId > 5)) { // Ici la machine 0 est ignorée cela prends un peu plus de mémoire mais cela est plus simple a modéliser dans le cas de cet exercice
                                          println("Veuillez choisir une machine entre 1 et 5 SVP.")
                                          machineId = readLine("> ").toInt
                                    }
                                    if (!validatePin(machineId, MachinePins)){
                                          quitter = true
                                    }else {
                                          println("1) Tapez 1 pour changer de PIN\n2) Tapez 2 pour vérifier les stocks")
                                          var choixadmin = readLine("> ").toInt
                                          while (choixadmin != 1 && choixadmin != 2) {
                                                println("1) Tapez 1 pour changer de PIN\n2) Tapez 2 pour modifier les stocks")
                                                choixadmin = readLine("> ").toInt
                                          }
                                          if (choixadmin == 1) {
                                                updatePin(machineId, MachinePins)
                                          }
                                          if (choixadmin == 2) {
                                                restockMachine(machineId, coffeStocks, sugarStocks, milkStocks)
                                          }
                                    }
                              }
                              if(choix == 3){
                                    println("Merci de votre visite et à bientôt.")
                                    quitter = true
                              }
                        }

                  }

      }

