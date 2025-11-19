import io.StdIn._
import scala.language.postfixOps
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    //Déclaration des variables

    //MENU PRINCIPAL (saisie)
    //Modes: 1) Client 2)Admin 3)Quitter
    var choixMode = 0

    //codes
    //code admin
    val codeAdmin = 434343
    //code Twint
    val codeTwint=Random.alphanumeric take 5 mkString


    //textes
    //Invites
    val txtNomSociete = "Nospresso Café"
    val txtChoixMode = "Veuillez sélectionner un mode : "
    val txtChoixBoisson = "Veuillez sélectionner votre boisson : "
    val txtChoixLatte = "Veuillez sélectionner la taille de votre Latte : "
    val txtSucre = "Souhaitez-vous ajouter du sucre ? "
    val txtLait = "Souhaitez-vous ajouter du lait en supplément ? "
    val txtDoseLait = "Combien de dose ? Max. 3 par boisson (1 dose : 50ml)"

    //Mode Admin
    val modeAdmin="Bienvenue au mode Admin !\nRéapprovisionnement des produits."
    val questionAdmin="Quel produit voulez-vous réapprovisionner ? : "
    val questionQuantProduit="Quelle quantité de produit voulez-vous ajouter ? "

    //textes modes
    val txtClient = "1) Client"
    val txtAdmin = "2) Admin"
    val txtQuitter = "3) Quitter"
    //textes choix boissons
    val txtExpresso = "1) Expresso - CHF 2.00"
    val txtCappuccino = "2) Cappuccino - CHF 2.50"
    val txtLatte = "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)"
    //textes choix sucre
    val txtSansSucre = "1) Sans sucre"
    val txtPeuSucre = "2) Peu (5g) - CHF 0.10"
    val txtMoySucre = "3) Moyen (10g) - CHF 0.20"
    val txtBcpSucre = "4) Beaucoup (15g) - CHF 0.30"
    //textes Menu choixLatte
    val txtLattePetit = "1) Latte (Petit)"
    val txtLatteMoyen = "2) Latte (Moyen)"
    val txtLatteGrand = "3) Latte (Grand)"

    //textes pour Latte écran de paiement
    val txtLattePetitfinal = "Latte (Petit)"
    val txtLatteMoyenfinal = "Latte (Moyen)"
    val txtLatteGrandfinal = "Latte (Grand)"

    //Textes erreurs
    //Stock insuffisant
    val poudreInsuffisante = "Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin."
    val laitInsuffisant = "Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus pétite ou essayer une autre boisson."
    val sucreInsuffisantDose= "Pas assez de sucre pour la dose sélectionnée. Veuillez choisir une dose plus petite ou réapprovisionner en mode Admin. "
    val sucreInsuffisantInitial= "Pas assez de sucre pour ajouter à la boisson. Veuillez en réapprovisionner en mode Admin. "
    val laitSuppInsuffisant="Pas assez de lait pour la dose sélectionnée. Veuillez sélectionner une dose plus petite ou en réapprovisionner en mode Admin. "

    // Choix non valable
    val choixNonValide = "Veuillez choisir une option valide."
    //textes écran de paiement
    var typeBoisson = " "
    //niveau sucre
    var niveauSucre = " "
    //lait en supplément
    var laitSupp = " "


    //Menu Admin (saisie)
    var choixProduitAdmin=0
    var quantiteProduit=0
    var quitterModeAdmin=0
    var codeSaisiAdmin=0

    //Menu Client (saisie)
    // choix boissons: 1)Expresso 2)Cappuccino 3)Latte
    var choixBoisson = 0
    // choix sucre: 1)Sans sucre 2)Peu 3)Moyen 4)Beaucoup
    var choixSucre = 0
    // choix Latte: 1)Petit 2)Moyen 3)Grand
    var choixLatte = 0
    // choix lait en supplément: 1)Oui 2)Non
    var choixLait = 0
    // choix dose de lait
    var nbDoseLait = 0

    //PRIX
    //prix boissons en CHF
    val prixExpresso = 2.00
    val prixCapuccino = 2.50
    val prixLattePetit = 2.70
    val prixLatteMoyen = 3.20
    val prixLatteGrand = 3.70
    var prixBoisson=0.0 //Variable qui va capturer le prix final de la boisson sans sucre ni lait en supp.
    //prix suppléments en CHF
    val prixPeuSucre = 0.10
    val prixMoyenSucre = 0.20
    val prixBcpSucre = 0.30
    val prixDoseLait = 0.05
    //pour le calcul du prix à la fin
    var prixSucre=0.0
    var prixLait=0.0
    //prix total
    var prixTotal = 0.0


    //STOCK INITIAL
    // stock en g et ml
    var poudreCafe = 50
    var sucre = 30
    var lait = 500
    var doseLaitDispo=0

    //Déductions stock
    //Poudre de cafe: quantité par boisson en g
    val poudreExpresso = 8
    val poudreCappuccino = 6
    val poudreLattePet = 6
    val poudreLatteMoy = 8
    val poudreLatteGra = 12
    //Lait: quantité par boisson en ml
    val laitCappuccino = 100
    val laitLattePet = 120
    val laitLatteMoy = 150
    val laitLatteGra = 200
    //Dose de lait
    val doseLait = 50

    //Sucre: quantité par niveau en g
    val peuSucre = 5
    val moySucre = 10
    val bcpSucre = 15


    //Fin déclaration de variables et début de la boucle

    do { //Boucle principale Tant que l'utilisateur ne quitte le programme choixMode!=3

      println(txtNomSociete)
      println(txtChoixMode)
      println(txtClient)
      println(txtAdmin)
      println(txtQuitter)
      choixMode = readLine("> ").toInt

      //validation mode et écran d'accueil
      while (choixMode<1 && choixMode>3){ //tant que le choix est différent aux trois choix valides 1) Client 2) Admin 3) Quitter
        println(choixNonValide) // válidation non valide
        choixMode = readLine("> ").toInt
        println()
      }

      //DEBUT MODE CLIENT

      if (choixMode == 1) { //Mode Client

        do { //

          //Menu client : choix de boissons
          println(txtNomSociete)
          println(txtChoixBoisson)
          println(txtExpresso)
          println(txtCappuccino)
          println(txtLatte)
          choixBoisson = readLine("> ").toInt

          while (choixBoisson<1 || choixBoisson>3) {
            println(choixNonValide) // válidation mauvais choix
            choixBoisson = readLine("> ").toInt
            println()
          }
          //Attribution du type de boisson
          if (choixBoisson==1) typeBoisson="Expresso"
          else if (choixBoisson==2) typeBoisson="Cappuccino"
          else if (choixBoisson==3) {
            println(txtChoixLatte)
            println(txtLattePetit)
            println(txtLatteMoyen)
            println(txtLatteGrand)
            choixLatte = readLine("> ").toInt

            while (choixLatte<1 || choixLatte>3) { //Validation choix Latte
              println(choixNonValide) // válidation mauvais choix
              choixLatte = readLine("> ").toInt
              println()
            }

          }


          //AJOUT DU SUCRE

          if (sucre>=5 ) { //Validation de boisson réussie qui permet de passer au menu sucre seulement s'il y en a assez pour la dose minime

            do {

              println(txtSucre) //Question
              println(txtSansSucre) //1
              println(txtPeuSucre)//2
              println(txtMoySucre)//3
              println(txtBcpSucre)//4
              choixSucre = readLine("> ").toInt

              while (choixSucre<1 || choixSucre>4) {
                println(choixNonValide) // válidation mauvais choix
                choixSucre = readLine("> ").toInt
                println()
              }

              //do { //Validation choix sucre

              //Attribution niveau de sucre
              if (choixSucre==1) niveauSucre="Sans sucre"
              else if (choixSucre==2) niveauSucre="Peu"
              else if (choixSucre==3) niveauSucre="Moyen"
              else if (choixSucre==4) niveauSucre="Beaucoup"


              if (choixSucre==3 && sucre<10){ // ifs pour valider si la dose de sucre choisie est plus grande que le sucre disponible
                println(sucreInsuffisantDose)//+"Sucre disponible :"+sucre+"g")
                println()
                choixSucre=0 //Réinitialisation de la variable qui permet retourner au menu sucre

              }else if (choixSucre==4 && sucre<15){
                println(sucreInsuffisantDose)//+"Sucre disponible :"+sucre+"g")
                println()
                choixSucre=0 //Réinitialisation de la variable qui permet retourner au menu sucre

              } else if (choixSucre == 2) { //Déduction sucre du stock et calcul du prix du sucre
                sucre -= peuSucre
                prixSucre = prixPeuSucre
              } else if (choixSucre == 3) {
                sucre -= moySucre
                prixSucre = prixMoyenSucre
              } else if (choixSucre == 4) {
                sucre -= bcpSucre
                prixSucre = prixBcpSucre
              } else if (choixSucre == 1) {
                sucre = sucre
              }

            } while (choixSucre==0)

            //Sinon, si pas assez de sucre pour la dose minime : message indiquant qu'il n'y a pas assez de sucre et retour au menu client

          } else if (sucre<5) {
            println()
            println("Boisson sélectionnée : " + typeBoisson)
            println()
            println(sucreInsuffisantInitial)
            choixBoisson=0
          } //FIN ELSE IF PAS ASSEZ DE SUCRE



          if (choixBoisson == 1) { //Expresso
            //Côntrole de stock
            if (poudreCafe <= 8) {
              println(poudreInsuffisante) //rupture de stock donc retour à l'écran d'accueil
              choixBoisson=0 //Réinitialisation de la variable pour éviter que le programme passe à l'écran du sucre et du lait.

            } else { //Boisson réussie - Déduction quantités du stock
              poudreCafe -= poudreExpresso
              prixBoisson = prixExpresso
              //typeBoisson = "Expresso"
            }

          } else if (choixBoisson == 2) { //Cappuccino
            //Côntrole de stock
            if (poudreCafe < 6 || lait<100) {
              if (poudreCafe<6) {
                println(poudreInsuffisante)
              } else  if (lait<100) {
                println(laitInsuffisant)
              }
              choixBoisson=0 //Réinitialisation de la variable pour éviter que le programme passe à l'écran du sucre et du lait.
              println()

            } else { //Boisson réussie - Déduction quantités du stock
              poudreCafe -= poudreCappuccino
              lait -= laitCappuccino
              prixBoisson = prixCapuccino
              typeBoisson = "Cappuccino"

            }

          }

          //Préparation du latte selon la taille choisie préalablement
          if (choixLatte == 1) { //Latte Petit

            typeBoisson=txtLattePetitfinal

            if (poudreCafe<6 || lait < 120) { //Vérification stock de café et lait pour au moins un latte petit
              choixBoisson=0 //Réinitialisation de la variable pour éviter que le programme passe à l'écran du sucre et du lait.
              choixLatte=0 //Réinitialisation de la variable
              if (poudreCafe<6) {
                println()
                println("Boisson sélectionnée : " + typeBoisson)
                println("Niveau de sucre : " + niveauSucre)
                println()
                println(poudreInsuffisante) //rupture de stock donc retour au menu client
                println()
              }
              if (lait<120) {
                println("Boisson sélectionnée : " + typeBoisson)
                println("Niveau de sucre : " + niveauSucre)
                println()
                println(laitInsuffisant) //rupture de stock donc retour au menu client
                println()
              }
            } else {
              poudreCafe -= poudreLattePet
              lait -= laitLattePet
              prixBoisson = prixLattePetit
            }


          } else if (choixLatte == 2) {
            typeBoisson=txtLatteMoyenfinal

            if (poudreCafe<8 || lait < 150) { //Vérification stock de café et lait pour au moins un latte petit
              choixBoisson=0 //Réinitialisation de la variable pour éviter que le programme passe à l'écran du sucre et du lait.
              choixLatte=0 //Réinitialisation de la variable
              if (poudreCafe<8) {
                println()
                println("Boisson sélectionnée : " + typeBoisson)
                println("Niveau de sucre : " + niveauSucre)
                println()
                println(poudreInsuffisante) //rupture de stock donc retour au menu client
                println()
              }
              if (lait<150) {
                println()
                println("Boisson sélectionnée : " + typeBoisson)
                println("Niveau de sucre : " + niveauSucre)
                println()
                println(laitInsuffisant) //rupture de stock donc retour au menu client
                println()
              }

            }  else { //Boisson réussie - Déduction des quantités utilisées du stock
              poudreCafe -= poudreLatteMoy
              lait -= laitLatteMoy
              prixBoisson = prixLatteMoyen
            }
          } else if (choixLatte == 3) {

            typeBoisson=txtLatteGrandfinal

            if (poudreCafe<12 || lait < 200) { //Vérification stock de café et lait pour au moins un latte petit
              choixBoisson = 0 //Réinitialisation de la variable pour éviter que le programme passe à l'écran du sucre et du lait.
              choixLatte = 0 //Réinitialisation de la variable
              if (poudreCafe < 12) {
                println()
                println("Boisson sélectionnée : " + typeBoisson)
                println("Niveau de sucre : " + niveauSucre)
                println()
                println(poudreInsuffisante) //rupture de stock donc retour au menu client
              } else if (lait<200) {
                println()
                println("Boisson sélectionnée : " + typeBoisson)
                println("Niveau de sucre : " + niveauSucre)
                println()
                println(laitInsuffisant) //rupture de stock donc retour au menu client
              }

            } else {

              poudreCafe -= poudreLatteGra
              lait -= laitLatteGra
              prixBoisson = prixLatteGrand
            }
          }

          //} //Fin choix taille Latte



          //AJOUT DE LAIT EN SUPPLÉMENT

          //Mise à jour des doses de lait disponibles
          doseLaitDispo=lait/50
          //Validation pour les boissons sur lesquelles on peut ajouter du lait en supp.
          if ((choixBoisson == 2 || choixBoisson == 3) && doseLaitDispo>=1) { //Si le client choisit un Cappuccino ou un Latte, le programme propose le lait supplémentaire
            println(txtLait)
            println("1)Oui")
            println("2)Non")
            choixLait = readLine("> ").toInt

            while (choixLait<1 || choixLait>2) {
              println(choixNonValide) // válidation mauvais choix
              choixLait = readLine("> ").toInt
              println()
            }

            if (choixLait == 1) { //Si le client veut ajouter du lait... message demandant la dose

              println(txtDoseLait)//combien de doses ?
              nbDoseLait = readLine("> ").toInt

              while (nbDoseLait>doseLaitDispo || nbDoseLait>3){ //Tant que la dose saisie est plus grand que les doses disponibles...
                println(laitSuppInsuffisant) //message indiquant qu'il n'y pas assez de lait disponible et qu'il faut choisir une autre dose ou réapprovisionner en mode Admin
                nbDoseLait = readLine("> ").toInt
                println()

              }
              //Si la dose de lait choisie est plus petite que la dose dispo...le programme continue et fait la deduction du stock et le calcul du prix
              lait -= (doseLait * nbDoseLait)
              prixLait = prixDoseLait * nbDoseLait
              laitSupp = "Oui"

            } else if (choixLait == 2) {
              laitSupp = "Non"
            }
          } else if ((choixBoisson == 2 || choixBoisson == 3) && doseLaitDispo<1){//Si pas assez de lait pour ajouter en supplément
            println()
            println("Boisson sélectionnée : " + typeBoisson)
            println("Niveau de sucre : " + niveauSucre)
            println("Pas assez de lait pour ajouter en supplément.")
            println()
            laitSupp = "Non"
            choixLait=2
          } //Fin válidation lait en supp.


          //FIN CHOIX BOISSONS


          //ÉCRAN DE PAIEMENT*******
          if (choixBoisson==1 || choixBoisson==2 || choixBoisson==3) {

            prixTotal=prixBoisson+prixSucre+prixLait //Addition du prix de la boisson, du sucre et du suppl.

            println("Boisson sélectionnée : " + typeBoisson)
            println("Niveau de sucre : " + niveauSucre)

            //Validation et calcul prix boissons et suppléments
            if ((choixBoisson==2 || choixBoisson==3) && (choixSucre==1 && choixLait==2)) { //Total pour Cappuccino et Latte sans sucre ni lait
              println("Lait supplémentaire : "+laitSupp)
              printf("Prix total : CHF %.2f",prixTotal)
              println()
            } else if ((choixBoisson==2 || choixBoisson==3) && (choixSucre!=1 && choixLait==2)) { //Total pour Cappuccino et Latte sans lait mais avec du sucre
              println("Lait supplémentaire : "+laitSupp)
              printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixBoisson, prixSucre,prixTotal)
              println()
            } else if ((choixBoisson==2 || choixBoisson==3) && (choixSucre==1 && choixLait==1)) { //Total pour Cappuccino et Latte sans sucre mais avec du lait
              println("Lait supplémentaire : "+laitSupp)
              printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixBoisson,prixLait,prixTotal)
              println()
            } else if (choixBoisson==1 && choixSucre==1) { //Total pour expresso sans sucre
              printf("Prix total : CHF %.2f",prixTotal)
              println()
            }else if (choixBoisson==1 && choixSucre!=1) { //Total expresso avec du sucre
              printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixBoisson, prixSucre,prixTotal)
              println()
            } else { //Total pour les boissons avec sucre et lait 6
              println("Lait supplémentaire : "+laitSupp)
              printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixBoisson, prixSucre, prixLait,prixTotal)
              println()
            }

            println("Veuillez payer en utilisant Twint. \nVotre code de paiement est : "+codeTwint)
            println("(En attente de validation de paiement...)")
            println()
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté.\nPréparation de votre boisson...")
            println()
            Thread.sleep(2000)
            println("Votre "+typeBoisson+" est prêt ! Bonne dégustation !")
            println()
            Thread.sleep(3000)

            //Réinitilaisation de la variable pour revenir au menu Modes
            choixMode=0
            // Réinitialisation des variables pour éviter qu'elles gardent les valeurs attribuées dans la boucle dans le cas d'une boisson réussie
            if (choixMode==0) {
              choixLatte=0
              choixSucre=0
              choixLait=0
            }


          } //Fin if principal paiement

          //Fin Messages écran de paiement *******

          //Réinitialisation des variables pour éviter qu'elles gardent les valeurs attribuées dans la boucle dans le cas d'une boisson pas réussie
          if (choixBoisson==0) {
            choixLatte=0
            choixSucre=0
            choixLait=0
          }


        }while (choixBoisson==0) //FIN boucle CLIENT.


        //DEBUT MODE ADMIN

      } else if (choixMode==2) {

        do { //Demander le code d'accès la première fois ou si le code serait erronée et que l'utilisateur veuille ressayer

          println("Veuillez saisir le code d'accès : ")
          codeSaisiAdmin=readLine("> ").toInt

          if ((codeSaisiAdmin==codeAdmin) && quitterModeAdmin!=1) { //si le code d'accès est juste
            println(modeAdmin)
            println("Niveau des stocks : ")
            println("Poudre de café : "+poudreCafe+"g")
            println("Sucre : "+sucre+"g")
            println("Lait : "+lait+"ml")
            println()
            Thread.sleep(3000)

            do { //Demander le produit à stocker tant que l'utilisateur ne quite le mode Admin

              println(questionAdmin)
              println("1)Poudre de café (g)")
              println("2)Sucre (g)")
              println("3)Lait (ml)")
              println("4)Quitter")
              choixProduitAdmin=readLine("> ").toInt

              while (choixProduitAdmin<1 || choixProduitAdmin>4) { //Demander le type de produit tant que l'utilisateur ne choisisse une option valide
                println(choixNonValide)
                choixProduitAdmin=readLine("> ").toInt
                println()

              }

              //Validations de choix avec if
              if (choixProduitAdmin==1) {  //Poudre de cafe
                println("Produit sélectionné : Poudre de café. Niveau actuel : "+poudreCafe+"g")
                println(questionQuantProduit)
                quantiteProduit=readLine("> ").toInt
                poudreCafe+=quantiteProduit
                println("Niveau de poudre de café après approvisionnement : "+poudreCafe+"g")
                choixProduitAdmin=0 //Réinitialisation variable permettant sortir du if
              } else if (choixProduitAdmin==2) { //Sucre
                println("Produit sélectionné : Sucre. Niveau actuel : "+sucre+"g")
                println(questionQuantProduit)
                quantiteProduit=readLine("> ").toInt
                sucre+=quantiteProduit
                println("Niveau de sucre après approvisionnement : "+sucre+"g")
                choixProduitAdmin=0 //Réinitialisation variable
              }else if  (choixProduitAdmin==3) { //Lait
                println("Produit sélectionné : Lait (ml). Niveau actuel : "+lait+"ml")
                println(questionQuantProduit)
                quantiteProduit=readLine("> ").toInt
                lait+=quantiteProduit
                println("Niveau de lait après approvisionnement : "+lait+"ml")
                choixProduitAdmin=0 //Réinitialisation variable
              } else if (choixProduitAdmin==4) { //Quitter le mode admin
                quitterModeAdmin=1
              }

              if (quitterModeAdmin!=1) { //pour quitter le mode admin au cas où on aurait réapprovisionné des produits
                println("Voulez vous quitter le mode Admin ou sélectionner un autre produit à réapprovisionner ? ")
                println("1) Quitter")
                println("2) Réapprovisionner un autre produit")
                quitterModeAdmin=readLine("> ").toInt

                while (quitterModeAdmin<1 || quitterModeAdmin>2) {
                  println(choixNonValide)
                  quitterModeAdmin=readLine("> ").toInt
                  println()
                }

              }

            } while (quitterModeAdmin!=1) //Fin boucle produits à restocker


          } else if (codeSaisiAdmin!=codeAdmin && quitterModeAdmin!=1){ //Si le code d'accès n'est pas juste
            println("Code erroné.")
            println("Voulez-vous quitter ou ressayer le code ? ") //Proposition de quitter le mode ou ressayer le code
            println("1)Quitter")
            println("2)Ressayer")
            quitterModeAdmin=readLine("> ").toInt
            if (quitterModeAdmin<1 || quitterModeAdmin>2) {
              println(choixNonValide)
            }
            println()
          }



        }while (quitterModeAdmin==2) //Fin boucle pour demander le code


        //Contrôle stock
        println("Niveau des stocks : ")
        println("Poudre de café : "+poudreCafe+"g")
        println("Sucre : "+sucre+"g")
        println("Lait : "+lait+"ml")
        println()

        //Reinitialisation variables du Mode Admin
        choixProduitAdmin=0
        quantiteProduit=0
        quitterModeAdmin=0

      }


      //Réinitialisation variable qui permet rester sortir de la boucle admin
      quitterModeAdmin=0


    } while (choixMode!=3) //Tant que choixMode soit égale égale à 0 (valeur initiale) et différent à 3 (valeur qui sert à quitter le programme) la boucle s'exécutera.

    println("Merci ! Au revoir ! ")



  }//Fin du objet Main





}//Fin du programme