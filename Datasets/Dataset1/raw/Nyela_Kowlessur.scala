import io.StdIn._
import scala.util.Random
object Main {
  def main(args: Array[String]): Unit = {
    // notion utiles : Util.Random
    //printf("Le prix de la boisson est de : %.2f\n", PrixTotal)
    var mode1 = 3
    var PrixTotal = 0.0
    var TypeBoisson = 0.0
    var sucre = 0.0
    var lait = 0.0
    var TailleLatte = 0.0
    var codeadmin = 434343

    // definition des quantités dans la machine
    var QuantiteCafeMachine = 50.0 //grammes
    var QuantiteSucreMachine = 30.0 //grammes
    var QuantiteLaitMachine = 0.5 // litres

    //définition stock après les commandes
    var StockSucre = 30.0
    var StockCafe = 50.0
    var StockLait = 0.5
    var ajout = 0.0
    var ajoutsucre = 0.0
    var ajoutlait = 0.0
    var ajoutcafe = 0.0
    // définition des prix de base
    var PrixBaseExresso = 2.00 //francs
    var PrixBaseCappuccino = 2.50 // francs
    var PrixBaseLattePetit = 2.70 // francs
    var PrixBaseLatteMoyen = 3.20 // francs
    var PrixBaseLatteGrand = 3.70 // francs

    // définition des prix des suppléments
    var SucrePrix = 0.10 // francs par 5g
    var LaitPrix = 0.05 // francs par 0.05L

    // définition qt suppléments
    var DoseSucre = 5 // grammes
    var DoseLait = 0.05 // Litres
    var DoseCafeExpresso = 8.0 //grammes
    var DoseCafeCappuccino = 6.0 //grammes
    var DoseCafeLattePetit = 6.0 //grammes
    var DoseCafeLatteMoyen = 8.0 //grammes
    var DoseCafeLatteGrand = 12 //grammes
    val LimiteDoseLait = 3 // dose limite de lait supplémentaire
    var NbDoseLait = 0 // nombre de doses
    var mode = 0

    var sortir = mode == 3
      println("\t\nNospresso Café")
    while (sortir == false) {
      mode = readLine("Veuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n> ").toInt

        if (mode == 1) {
          if (mode == 1) {
            var TypeBoisson = readLine("\nVeuillez sélectionner votre boisson : \n1) Expresso -  CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt

            while (StockCafe <= DoseCafeExpresso || StockCafe <= DoseCafeCappuccino || StockCafe <= DoseCafeLattePetit || StockCafe <= DoseCafeLatteMoyen || StockCafe <= DoseCafeLatteGrand || StockLait <= DoseLait || StockSucre <= DoseSucre) {
              if (StockCafe <= DoseCafeExpresso || StockCafe <= DoseCafeCappuccino || StockCafe <= DoseCafeLattePetit || StockCafe <= DoseCafeLatteMoyen || StockCafe <= DoseCafeLatteGrand) {
                println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (StockLait <= DoseLait) {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              } else if (StockSucre <= DoseSucre) {
                println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              }
              TypeBoisson = readLine("\nVeuillez sélectionner votre boisson : \n1) Expresso -  CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) \n> ").toInt
            }

            //Expresso
            if (TypeBoisson == 1) {
              var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt

              if (sucre == 2) {
                PrixTotal = PrixBaseExresso + SucrePrix
                LaitPrix = 0
                StockSucre = StockSucre - DoseSucre
                StockLait = StockLait
                StockCafe = StockCafe - DoseCafeExpresso
                println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Non")
                printf("Prix total : Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseExresso, SucrePrix, PrixTotal)
              } // sucre 2
              else if (sucre == 3) {
                PrixTotal = PrixBaseExresso + (SucrePrix * 2)
                LaitPrix = 0
                StockSucre = StockSucre - (2 * DoseSucre)
                StockLait = StockLait
                StockCafe = StockCafe - DoseCafeExpresso
                println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : Moyen : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
                printf("Prix total : Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseExresso, 2 * SucrePrix, PrixTotal)
              } // sucre 3
              else if (sucre == 4) {
                PrixTotal = PrixBaseExresso + (SucrePrix * 3)
                LaitPrix = 0
                StockSucre = StockSucre - (DoseSucre * 3)
                StockLait = StockLait
                StockCafe = StockCafe - DoseCafeExpresso
                println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : Beaucoup : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
                printf("Prix total : Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseExresso, 3 * SucrePrix, PrixTotal)
              } // sucre 4
              else if (sucre == 1) {
                PrixTotal = PrixBaseExresso
                LaitPrix = 0
                StockSucre = StockSucre
                StockLait = StockLait
                StockCafe = StockCafe - DoseCafeExpresso
                println("\nBoisson sélectionnée : Expresso \nNiveau de sucre : Sans : " + 0 * DoseSucre + "g. \nLait supplémentaire : Non")
                printf("Prix total : Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseExresso, 0 * SucrePrix, PrixTotal)
              } // sucre 1
            } // if Expresso

            // cappuccino
            else if (TypeBoisson == 2) {
              var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
              var lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt

              if (sucre == 2) {
                PrixTotal = PrixBaseCappuccino + SucrePrix
                StockSucre = StockSucre - DoseSucre
                StockCafe = StockCafe - DoseCafeCappuccino
                if (lait == 1) {
                  var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                  while (NbDoseLait > LimiteDoseLait) {
                    println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                    NbDoseLait = readLine("\nCombien de doses ?\n>1").toInt
                  }
                  println("Vous avez choisi " + NbDoseLait + " doses.")
                  LaitPrix = DoseLait * NbDoseLait
                  PrixTotal = PrixTotal + LaitPrix
                  StockLait = StockLait - (NbDoseLait * DoseLait)
                  println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Oui")
                  printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, SucrePrix, LaitPrix, PrixTotal)
                } // lait oui
                else if (lait == 2) {
                  LaitPrix = DoseLait * NbDoseLait
                  PrixTotal = PrixTotal
                  StockLait = StockLait - (NbDoseLait * DoseLait)
                  println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Non")
                  printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, SucrePrix, 0 * LaitPrix, PrixTotal)
                } //lait 2
              } // sucre 2

              else if (sucre == 3) {
                PrixTotal = PrixBaseCappuccino + 2 * SucrePrix
                StockSucre = StockSucre - (2 * DoseSucre)
                StockCafe = StockCafe - DoseCafeCappuccino
                if (lait == 1) {
                  var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                  while (NbDoseLait > LimiteDoseLait) {
                    println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                    NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                  }
                  println("Vous avez choisi " + NbDoseLait + " doses.")
                  LaitPrix = DoseLait * NbDoseLait
                  PrixTotal = PrixTotal + LaitPrix
                  StockLait = StockLait - (NbDoseLait * DoseLait)
                  println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Peu : " + 2 * DoseSucre + "g. \nLait supplémentaire : Oui")
                  printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 2 * SucrePrix, LaitPrix, PrixTotal)
                }
                else if (lait == 2) {
                  LaitPrix = DoseLait * NbDoseLait
                  PrixTotal = PrixTotal
                  StockLait = StockLait - (NbDoseLait * DoseLait)
                  println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Peu : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
                  printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 2 * SucrePrix, 0 * LaitPrix, PrixTotal)
                }
              } // sucre 3

              else if (sucre == 4) {
                PrixTotal = PrixBaseCappuccino + 3 * SucrePrix
                StockSucre = StockSucre - (3 * DoseSucre)
                StockCafe = StockCafe - DoseCafeCappuccino
                if (lait == 1) {
                  var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                  while (NbDoseLait > LimiteDoseLait) {
                    println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                    NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                  }
                  println("Vous avez choisi " + NbDoseLait + " doses.")
                  LaitPrix = DoseLait * NbDoseLait
                  PrixTotal = PrixTotal + LaitPrix
                  StockLait = StockLait - (NbDoseLait * DoseLait)
                  println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Peu : " + 3 * DoseSucre + "g. \nLait supplémentaire : Oui")
                  printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 3 * SucrePrix, LaitPrix, PrixTotal)
                }
                else if (lait == 2) {
                  LaitPrix = DoseLait * NbDoseLait
                  PrixTotal = PrixTotal
                  StockLait = StockLait - (NbDoseLait * DoseLait)
                  println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Peu : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
                  printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 3 * SucrePrix, 0 * LaitPrix, PrixTotal)
                }
              } //sucre 4

              else if (sucre == 1) {
                PrixTotal = PrixBaseCappuccino + 0 * SucrePrix
                StockSucre = StockSucre - (0 * DoseSucre)
                StockCafe = StockCafe - DoseCafeCappuccino
                if (lait == 1) {
                  var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                  while (NbDoseLait > LimiteDoseLait) {
                    println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                    NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                  }
                  println("Vous avez choisi " + NbDoseLait + " doses.")
                  LaitPrix = DoseLait * NbDoseLait
                  PrixTotal = PrixTotal + LaitPrix
                  StockLait = StockLait - (NbDoseLait * DoseLait)
                  println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Peu : " + 0 * DoseSucre + "g. \nLait supplémentaire : Oui")
                  printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 0 * SucrePrix, LaitPrix, PrixTotal)
                }
                else if (lait == 2) {
                  LaitPrix = DoseLait * NbDoseLait
                  PrixTotal = PrixTotal
                  StockLait = StockLait - (NbDoseLait * DoseLait)
                  println("\nBoisson sélectionnée : Cappuccino \nNiveau de sucre : Peu : " + 0 * DoseSucre + "g. \nLait supplémentaire : Non")
                  printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseCappuccino, 0 * SucrePrix, 0 * LaitPrix, PrixTotal)
                }
              } //sucre 1
            } // Cappuccino

            //Latte
            else if (TypeBoisson == 3) {
              var TailleLatte = readLine("\nVeuillez sélectionner la taille de votre Latte : \n1) Petit - CHF 2.70 \n2) Moyen - CHF 3.20 \n3) Grand - CHF 3.70 \n> ").toInt

              if (TailleLatte == 1) {
                var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
                var lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt

                if (sucre == 2) {
                  PrixTotal = PrixBaseLattePetit + SucrePrix
                  StockSucre = StockSucre - DoseSucre
                  StockCafe = StockCafe - DoseCafeLattePetit
                  if (lait == 1) {
                    var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    while (NbDoseLait > LimiteDoseLait) {
                      println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                      NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    }
                    println("Vous avez choisi " + NbDoseLait + " doses.")
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal + LaitPrix
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Oui")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, SucrePrix, LaitPrix, PrixTotal)
                  } // lait oui
                  else if (lait == 2) {
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Non")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, SucrePrix, 0 * LaitPrix, PrixTotal)
                  } //lait 2
                } //sucre 2

                else if (sucre == 3) {
                  PrixTotal = PrixBaseLattePetit + 2 * SucrePrix
                  StockSucre = StockSucre - (2 * DoseSucre)
                  StockCafe = StockCafe - DoseCafeLattePetit
                  if (lait == 1) {
                    var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    while (NbDoseLait > LimiteDoseLait) {
                      println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                      NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    }
                    println("Vous avez choisi " + NbDoseLait + " doses.")
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal + LaitPrix
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Peu : " + 2 * DoseSucre + "g. \nLait supplémentaire : Oui")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 2 * SucrePrix, LaitPrix, PrixTotal)
                  }
                  else if (lait == 2) {
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Peu : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 2 * SucrePrix, 0 * LaitPrix, PrixTotal)
                  }
                } // sucre 3

                else if (sucre == 4) {
                  PrixTotal = PrixBaseLattePetit + 3 * SucrePrix
                  StockSucre = StockSucre - (3 * DoseSucre)
                  StockCafe = StockCafe - DoseCafeLattePetit
                  if (lait == 1) {
                    var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    while (NbDoseLait > LimiteDoseLait) {
                      println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                      NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    }
                    println("Vous avez choisi " + NbDoseLait + " doses.")
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal + LaitPrix
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Peu : " + 3 * DoseSucre + "g. \nLait supplémentaire : Oui")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 3 * SucrePrix, LaitPrix, PrixTotal)
                  }
                  else if (lait == 2) {
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Peu : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 3 * SucrePrix, 0 * LaitPrix, PrixTotal)
                  }
                } //sucre 4

                else if (sucre == 1) {
                  PrixTotal = PrixBaseLattePetit + 0 * SucrePrix
                  StockSucre = StockSucre - (0 * DoseSucre)
                  StockCafe = StockCafe - DoseCafeLattePetit
                  if (lait == 1) {
                    var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    while (NbDoseLait > LimiteDoseLait) {
                      println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                      NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    }
                    println("Vous avez choisi " + NbDoseLait + " doses.")
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal + LaitPrix
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Peu : " + 0 * DoseSucre + "g. \nLait supplémentaire : Oui")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 0 * SucrePrix, LaitPrix, PrixTotal)
                  }
                  else if (lait == 2) {
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Petit) \nNiveau de sucre : Peu : " + 0 * DoseSucre + "g. \nLait supplémentaire : Non")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLattePetit, 0 * SucrePrix, 0 * LaitPrix, PrixTotal)
                  }
                }
              } //petit latte

              else if (TailleLatte == 2) {
                var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
                var lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt

                if (sucre == 2) {
                  PrixTotal = PrixBaseLatteMoyen + SucrePrix
                  StockSucre = StockSucre - DoseSucre
                  StockCafe = StockCafe - DoseCafeLatteMoyen
                  if (lait == 1) {
                    var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    while (NbDoseLait > LimiteDoseLait) {
                      println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                      NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    }
                    println("Vous avez choisi " + NbDoseLait + " doses.")
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal + LaitPrix
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Moyen) \nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Oui")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, SucrePrix, LaitPrix, PrixTotal)
                  } // lait oui
                  else if (lait == 2) {
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Peu : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, SucrePrix, 0 * LaitPrix, PrixTotal)
                  } //lait 2
                } //sucre 2

                else if (sucre == 3) {
                  PrixTotal = PrixBaseLatteMoyen + 2 * SucrePrix
                  StockSucre = StockSucre - (2 * DoseSucre)
                  StockCafe = StockCafe - DoseCafeLatteMoyen
                  if (lait == 1) {
                    var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    while (NbDoseLait > LimiteDoseLait) {
                      println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                      NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    }
                    println("Vous avez choisi " + NbDoseLait + " doses.")
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal + LaitPrix
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Moyen) \nNiveau de sucre : Peu : " + 2 * DoseSucre + "g. \nLait supplémentaire : Oui")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 2 * SucrePrix, LaitPrix, PrixTotal)
                  }
                  else if (lait == 2) {
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Moyen) \nNiveau de sucre : Peu : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 2 * SucrePrix, 0 * LaitPrix, PrixTotal)
                  }
                } // sucre 3

                else if (sucre == 4) {
                  PrixTotal = PrixBaseLatteMoyen + 3 * SucrePrix
                  StockSucre = StockSucre - (3 * DoseSucre)
                  StockCafe = StockCafe - DoseCafeLatteMoyen
                  if (lait == 1) {
                    var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    while (NbDoseLait > LimiteDoseLait) {
                      println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                      NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    }
                    println("Vous avez choisi " + NbDoseLait + " doses.")
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal + LaitPrix
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Peu : " + 3 * DoseSucre + "g. \nLait supplémentaire : Oui")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 3 * SucrePrix, LaitPrix, PrixTotal)
                  }
                  else if (lait == 2) {
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Peu : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 3 * SucrePrix, 0 * LaitPrix, PrixTotal)
                  }
                } //sucre 4

                else if (sucre == 1) {
                  PrixTotal = PrixBaseLatteMoyen + 0 * SucrePrix
                  StockSucre = StockSucre - (0 * DoseSucre)
                  StockCafe = StockCafe - DoseCafeLatteMoyen
                  if (lait == 1) {
                    var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    while (NbDoseLait > LimiteDoseLait) {
                      println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                      NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    }
                    println("Vous avez choisi " + NbDoseLait + " doses.")
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal + LaitPrix
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Peu : " + 0 * DoseSucre + "g. \nLait supplémentaire : Oui")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 0 * SucrePrix, LaitPrix, PrixTotal)
                  }
                  else if (lait == 2) {
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Moyen)\nNiveau de sucre : Peu : " + 0 * DoseSucre + "g. \nLait supplémentaire : Non")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteMoyen, 0 * SucrePrix, 0 * LaitPrix, PrixTotal)
                  }
                } //sucre 1

              } //moyen latte
              else if (TailleLatte == 3) {
                var sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHf 0.30 \n>").toInt
                var lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ? \n(Disponible uniquement pour Cappuccino et Latte) \n1) Oui \n2) Non \n>").toInt

                if (sucre == 2) {
                  PrixTotal = PrixBaseLatteGrand + SucrePrix
                  StockSucre = StockSucre - DoseSucre
                  StockCafe = StockCafe - DoseCafeLatteGrand
                  if (lait == 1) {
                    var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    while (NbDoseLait > LimiteDoseLait) {
                      println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                      NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    }
                    println("Vous avez choisi " + NbDoseLait + " doses.")
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal + LaitPrix
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Oui")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, SucrePrix, LaitPrix, PrixTotal)
                  } // lait oui
                  else if (lait == 2) {
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Peu : " + DoseSucre + "g. \nLait supplémentaire : Non")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, SucrePrix, 0 * LaitPrix, PrixTotal)
                  } //lait 2
                } //sucre 2

                else if (sucre == 3) {
                  PrixTotal = PrixBaseLatteGrand + 2 * SucrePrix
                  StockSucre = StockSucre - (2 * DoseSucre)
                  StockCafe = StockCafe - DoseCafeLatteGrand
                  if (lait == 1) {
                    var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    while (NbDoseLait > LimiteDoseLait) {
                      println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                      NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    }
                    println("Vous avez choisi " + NbDoseLait + " doses.")
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal + LaitPrix
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Peu : " + 2 * DoseSucre + "g. \nLait supplémentaire : Oui")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 2 * SucrePrix, LaitPrix, PrixTotal)
                  }
                  else if (lait == 2) {
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Peu : " + 2 * DoseSucre + "g. \nLait supplémentaire : Non")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 2 * SucrePrix, 0 * LaitPrix, PrixTotal)
                  }
                } // sucre 3

                else if (sucre == 4) {
                  PrixTotal = PrixBaseLatteGrand + 3 * SucrePrix
                  StockSucre = StockSucre - (3 * DoseSucre)
                  StockCafe = StockCafe - DoseCafeLatteGrand
                  if (lait == 1) {
                    var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    while (NbDoseLait > LimiteDoseLait) {
                      println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                      NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    }
                    println("Vous avez choisi " + NbDoseLait + " doses.")
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal + LaitPrix
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Peu : " + 3 * DoseSucre + "g. \nLait supplémentaire : Oui")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 3 * SucrePrix, LaitPrix, PrixTotal)
                  }
                  else if (lait == 2) {
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Peu : " + 3 * DoseSucre + "g. \nLait supplémentaire : Non")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 3 * SucrePrix, 0 * LaitPrix, PrixTotal)
                  }
                } //sucre 4

                else if (sucre == 1) {
                  PrixTotal = PrixBaseLatteGrand + 0 * SucrePrix
                  StockSucre = StockSucre - (0 * DoseSucre)
                  StockCafe = StockCafe - DoseCafeLatteGrand
                  if (lait == 1) {
                    var NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    while (NbDoseLait > LimiteDoseLait) {
                      println("Les doses supplémentaires maximum sont de " + LimiteDoseLait)
                      NbDoseLait = readLine("\nCombien de doses ?\n>").toInt
                    }
                    println("Vous avez choisi " + NbDoseLait + " doses.")
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal + LaitPrix
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Peu : " + 0 * DoseSucre + "g. \nLait supplémentaire : Oui")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 0 * SucrePrix, LaitPrix, PrixTotal)
                  }
                  else if (lait == 2) {
                    LaitPrix = DoseLait * NbDoseLait
                    PrixTotal = PrixTotal
                    StockLait = StockLait - (NbDoseLait * DoseLait)
                    println("\nBoisson sélectionnée : Latte (Grand)\nNiveau de sucre : Peu : " + 0 * DoseSucre + "g. \nLait supplémentaire : Non")
                    printf("Prix total : Chf %.2f + Chf %.2f + Chf %.2f = Chf %.2f", PrixBaseLatteGrand, 0 * SucrePrix, 0 * LaitPrix, PrixTotal)
                  } // lait 2
                } // sucre 1
              } // grand latte
            } // Latte

            //paiement twint
            println('\n')
            val caracteres = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            val random = new Random()
            var codetwint = " "
            val length = 5
            for (_ <- 1 to length) {
              val caracteresAleatoire = caracteres(random.nextInt(caracteres.length))
              codetwint += caracteresAleatoire
              println("Veuillez payer en utilisant Twint.\nVotre code de paiement est : " + codetwint + "\n(En attente de validation du paiement...)\n")
            }
            println("Paiement confirmé.\nPréparation de votre boisson...")
            Thread.sleep(3000)

            if (TypeBoisson == 1) {
              println("Votre Expresso est prêt ! Bonne dégustation !\n")
            } else if (TypeBoisson == 2) {
              println("Votre Cappuccino est prêt ! Bonne dégustation !\n")
            } else if (TypeBoisson == 3) {
              println("Votre Latte est prêt ! Bonne dégustation !\n")
            }
          }
        } // mode 1

        else if (mode == 2) {
          println("\nMode Admin")
          var codeadmin = readLine("Entrez le code PIN : ").toInt

          while (codeadmin != 434343) {
            println("Accès refusé")
            codeadmin = readLine("Entrez le code PIN : ").toInt
          }
          println("Accès autorisé \n\nStocks :  \n\tPoudre de café : " + StockCafe + "g. \n\tLait : " + StockLait + "L. \n\tSucre : " + StockSucre + "g.\n")
          ajout = readLine("Souhaitez-vous réajuster les stocks ? : \n1) Oui \n2) Non \n>").toInt

          if (ajout == 1) {
            while (ajout != 4) { // boucle pour réapprovisionnement
              ajout = readLine("\nSouhaitez-vous réapprovisionner : \n1) Sucre \n2) Poudre de café \n3) Lait \n4) Quitter \n>").toInt
              println("\nRéapprovisionnement des stocks ... \nAjout : \n")

              if (ajout == 2) {
                ajoutcafe = readLine("Quelle quantité de café souhaitez-vous rajouter ?").toInt
                if (ajoutcafe < QuantiteCafeMachine - StockCafe) {
                  StockCafe = StockCafe + ajoutcafe
                  println("\t Poudre de café : " + StockCafe)
                } else if (ajoutcafe > QuantiteCafeMachine - StockCafe) {
                  println("La quantité à ajouter ne doit pas excéder " + (QuantiteCafeMachine - StockCafe) + " grammes.")
                }
                StockLait = StockLait
                println("\t Lait : " + StockLait)
                StockSucre = StockSucre
                println("\t Sucre : " + StockSucre)

              } else if (ajout == 3) {
                ajoutlait = readLine(("Quelle quantité de lait souhaitez-vous rajouter ?")).toInt
                if (ajoutlait < QuantiteLaitMachine - StockLait) {
                  StockLait = StockLait + ajoutlait
                  println("\t Lait : " + StockLait)
                } else if (ajoutlait > QuantiteLaitMachine - StockLait) {
                  println("La quantité à ajouter ne doit pas excéder " + (QuantiteLaitMachine - StockLait) + " litres.")
                }
                StockCafe = StockCafe
                println("\t Café : " + StockCafe)
                StockSucre = StockSucre
                println("\t Sucre : " + StockSucre)
              }
              else if (ajout == 1) {
                ajoutsucre = readLine(("Quelle quantité de sucre souhaitez-vous rajouter ?")).toInt
                if (ajoutsucre < QuantiteSucreMachine - StockSucre) {
                  StockSucre = StockSucre + ajoutsucre
                  println("\t Sucre : " + StockSucre)
                } else if (ajoutsucre > QuantiteSucreMachine - StockSucre) {
                  println("La quantité à ajouter ne doit pas excéder " + (QuantiteSucreMachine - StockSucre) + " grammes.")
                }
                StockCafe = StockCafe
                println("\t Café : " + StockCafe)
                StockLait = StockLait
                println("\t Lait : " + StockLait)
              } else {
              }

              println("Niveaux de stocks mis à jour.")
            }
          }
          println("Retour au menu principal...")
        } else {
          sortir = true

        } // mode 2

    } //Do du début
  } // Array
}// main


