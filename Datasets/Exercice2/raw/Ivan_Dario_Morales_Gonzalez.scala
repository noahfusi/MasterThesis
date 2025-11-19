import io.StdIn._
import scala.util.Random
import scala.language.postfixOps

object Main {

  //Déclarations
  //Valeurs et variables
  val nbMachines=5

  //Tableaux
  //Codes PIN
  var machinePins: Array[Int] =Array.fill(nbMachines)(434343)
  //Stocks
  var coffeStocks: Array[Int] =Array.fill(nbMachines)(50)
  var sugarStocks: Array[Int] =Array.fill(nbMachines)(30)
  var milkStocks: Array[Int] =Array.fill(nbMachines)(500)


  //MENU PRINCIPAL (saisie)
  //Machines
  var choixMachine: Int = 5
  //Modes: 1) Client 2)Admin 3)Quitter
  var choixMode = 0

  //codes
  //code Twint
  var codeTwint=" "


  //textes
  //Invites
  val txtNomSociete = "Nospresso Café"
  val txtChoixMachine="Veuillez sélectionner la machine à utiliser :\n1)Machine 1\n2)Machine 2\n3)Machine 3\n4)Machine 4\n5)Machine 5"
  val txtChoixMode = "Veuillez sélectionner un mode : "
  var mode=" "
  val txtChoixBoisson = "Veuillez sélectionner votre boisson : "
  val txtChoixLatte = "Veuillez sélectionner la taille de votre Latte : "
  val txtSucre = "Souhaitez-vous ajouter du sucre ? "
  val txtLait = "Souhaitez-vous ajouter du lait en supplément ? "
  val txtDoseLait = "Combien de dose ? Max. 3 par boisson (1 dose : 50ml)"

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
  val poudreInsuffisante = "Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre machine ou réapprovisionner les stocks en mode Admin."
  val laitInsuffisant = "Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus pétite ou essayer un boisson sans lait ou sélectionner une autre machine."
  val sucreInsuffisantDose= "Pas assez de sucre pour la dose sélectionnée. Veuillez choisir une dose plus petite ou en réapprovisionner en mode Admin. "
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
  var choixAdmin=0
  var codeSaisiAdmin=0
  var tentatives=3

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


  //Fin déclaration de variables


  //MÉTHODES ************************************************************

  def validatePin(machineId: Int, machinePins: Array[Int]): Boolean={
    println("Machine sélectionnée (1-5) > "+(machineId+1)+".")
    codeSaisiAdmin=readLine("Entrez le code PIN > ").toInt
    tentatives-=1
    while (codeSaisiAdmin!=machinePins(machineId) && tentatives>0) {
      println("Code PIN incorrect. "+tentatives+ " tentatives restantes.")
      codeSaisiAdmin=readLine("> ").toInt
      tentatives -=1
      if (tentatives==0 && codeSaisiAdmin!=machinePins(machineId)){
        println()
        println()
        println("Trop de tentatives échouées. Fin du programme.")
        println()
        choixMode=3
      }
    }
    if (codeSaisiAdmin==machinePins(machineId)) {
      println()
      println("Accès accordé.")
      Thread.sleep(2000)
      tentatives=3 //Tentatives pour l'accès au mode admin réinitialisées
      true
    } else false

  }

  def updatePin(machineId: Int, machinePins: Array[Int]): Unit={
    println("Mise à jour du code PIN pour la Machine "+(machineId+1)+".")
    Thread.sleep(2000)
    var nouveauCode = readLine(("Entrez un nouveau code PIN à 6 chiffres > ")).toInt
    var taille= nouveauCode.toString.length
    while (taille!=6){
      nouveauCode = readLine(("Entrez un nouveau code PIN à 6 chiffres > ")).toInt
      taille= nouveauCode.toString.length
    }
    machinePins(machineId) = nouveauCode
    println()
    println("Le code PIN a été mis à jour avec succès.")
    Thread.sleep(2000)
    println("Retour au menu principal...")
    println()
    println()
    Thread.sleep(3000)
    choixMode=0
  }


  def restockMachine(machineId: Int, coffeeStocks: Array[Int],sugarStocks: Array[Int], milkStocks: Array[Int]): Unit={
    println("Niveau des stocks pour la Machine "+(machineId+1)+" : ")
    println("Poudre de café : "+coffeeStocks(machineId)+"g")
    println("Sucre : "+sugarStocks(machineId)+"g")
    println("Lait : "+milkStocks(machineId)/1000.toDouble+"L")
    println()
    Thread.sleep(3000)

    var coffe=0
    var sugar=0
    var milk=0


    println("Entrez les quantités à ajouter : ")
    coffe+=readLine("Poudre de café (en g) > ").toInt
    while (coffe<0){
      println("Veuillez saisir une quantité valide.")
      coffe=readLine("Poudre de café (en g) > ").toInt
    }
    coffeeStocks(machineId)+=coffe
    sugar=readLine("Sucre (en g) > ").toInt
    while (sugar<0){
      println("Veuillez saisir une quantité valide.")
      sugar=readLine("Sucre (en g) > ").toInt
    }
    sugarStocks(machineId)+=sugar
    milk=readLine("Lait (en ml) > ").toInt
    while (milk<0){
      println("Veuillez saisir une quantité valide.")
      milk=readLine("Lait (en ml) > ").toInt
    }
    milkStocks(machineId)+=milk
    println("Les stocks ont été mis à jour avec succès.")
    println()
    println()
    Thread.sleep(2000)
    //Contrôle stock
    println("Niveau des stocks actuel : ")
    println("Poudre de café : "+coffeeStocks(machineId)+"g")
    println("Sucre : "+sugarStocks(machineId)+"g")
    println("Lait : "+milkStocks(machineId)/1000.toDouble+"L")
    Thread.sleep(2000)
    println()
    println("Retour au menu principal...")
    println()
    println()
    Thread.sleep(3000)
    choixMode=0
  }



  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean={

    if (coffeeStocks(machineId)<6 || sugarStocks(machineId)<5) {
    println("Niveau des stocks insuffisant pour préparer la boisson.\nVeuillez sélectionner une autre machine.")
      return false
    }
      //Menu client : choix de boissons

    do {


      println(txtNomSociete)
      println(txtChoixBoisson)
      println(txtExpresso)
      println(txtCappuccino)
      println(txtLatte)
      choixBoisson = readLine("> ").toInt

      while (choixBoisson<1 || choixBoisson>3) {
        println(choixNonValide) // válidation mauvais choix
        println()
        println(txtChoixBoisson)
        println(txtExpresso)
        println(txtCappuccino)
        println(txtLatte)
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
        if (choixLatte==1)  typeBoisson=txtLattePetitfinal
        else if (choixLatte==2)  typeBoisson=txtLatteMoyenfinal
        else if (choixLatte==3) typeBoisson=txtLatteGrandfinal
      }
      //AJOUT DU SUCRE

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

        //Attribution niveau de sucre
        if (choixSucre==1) niveauSucre="Sans sucre"
        else if (choixSucre==2) niveauSucre="Peu"
        else if (choixSucre==3) niveauSucre="Moyen"
        else if (choixSucre==4) niveauSucre="Beaucoup"


        if (choixSucre==3 && sugarStocks(machineId)<10){ // ifs pour valider si la dose de sucre choisie est plus grande que le sucre disponible
          println(sucreInsuffisantDose)//+"Sucre disponible :"+sucre+"g")
          println()
          choixSucre=0 //Réinitialisation de la variable qui permet retourner au menu sucre

        }else if (choixSucre==4 && sugarStocks(machineId)<15){
          println(sucreInsuffisantDose)//+"Sucre disponible :"+sucre+"g")
          println()
          choixSucre=0 //Réinitialisation de la variable qui permet retourner au menu sucre

        } else if (choixSucre == 2) { //Déduction sucre du stock et calcul du prix du sucre
          sugarStocks(machineId) -= peuSucre
          prixSucre = prixPeuSucre
        } else if (choixSucre == 3) {
          sugarStocks(machineId) -= moySucre
          prixSucre = prixMoyenSucre
        } else if (choixSucre == 4) {
          sugarStocks(machineId) -= bcpSucre
          prixSucre = prixBcpSucre
        } else if (choixSucre == 1) {
          sugarStocks(machineId) = sugarStocks(machineId)
          prixSucre=0
        }

      } while (choixSucre==0)


      if (choixBoisson == 1) { //Expresso
        //Côntrole de stock
        if (coffeeStocks(machineId) < 8) {
          println(poudreInsuffisante) //rupture de stock donc retour à l'écran d'accueil
          //Réinitialisation de la variable
          choixBoisson=0
          return false

        } else { //Boisson réussie - Déduction quantités du stock
          coffeeStocks(machineId) -= poudreExpresso

          prixBoisson = prixExpresso

        }

      } else if (choixBoisson == 2) { //Cappuccino
        //Côntrole de stock
        if (coffeeStocks(machineId)<6 || milkStocks(machineId)<100) {
          choixBoisson=0  //Réinitialisation des variables pour éviter que le programme ne continue au menu de lait supplémentaire.
                  if (coffeeStocks(machineId)<6) {
                    println(poudreInsuffisante)
                    println()
                  }
                  if (milkStocks(machineId)<100) {
                    println(laitInsuffisant)
                    println()
                  }
          return false
        } else { //Boisson réussie - Déduction quantités du stock
          coffeeStocks(machineId) -= poudreCappuccino
          milkStocks(machineId) -= laitCappuccino
          prixBoisson = prixCapuccino

        }

      } else if (choixBoisson==3) { //Préparation du latte selon la taille choisie préalablement

        if (choixLatte == 1) { //Latte Petit

          if (coffeeStocks(machineId)<6 || milkStocks(machineId) < 120) { //Vérification stock de café et lait pour au moins un latte petit
            //Réinitialisation de la variable pour éviter que le programme ne continue au menu de lait supplémentaire.
            choixBoisson=0
            if (coffeeStocks(machineId)<6) {
              println()
              println("Boisson sélectionnée : " + typeBoisson)
              println("Niveau de sucre : " + niveauSucre)
              println()
              println(poudreInsuffisante) //rupture de stock donc retour au menu client
              println()
            }
            if (milkStocks(machineId)<120) {
              println("Boisson sélectionnée : " + typeBoisson)
              println("Niveau de sucre : " + niveauSucre)
              println()
              println(laitInsuffisant) //rupture de stock donc retour au menu client
              println()
            }
            return false
          } else {
            coffeeStocks(machineId) -= poudreLattePet
            milkStocks(machineId) -= laitLattePet
            prixBoisson = prixLattePetit
          }


        } else if (choixLatte == 2) {

          if (coffeeStocks(machineId)<8 || milkStocks(machineId) < 150) { //Vérification stock de café et lait pour au moins un latte petit
            //Réinitialisation des variables pour éviter que le programme ne continue au menu de lait supplémentaire.
            choixBoisson=0

            if (coffeeStocks(machineId)<8) {
              println()
              println("Boisson sélectionnée : " + typeBoisson)
              println("Niveau de sucre : " + niveauSucre)
              println()
              println(poudreInsuffisante) //rupture de stock donc retour au menu client
              println()
            }
            if (milkStocks(machineId)<150) {
              println()
              println("Boisson sélectionnée : " + typeBoisson)
              println("Niveau de sucre : " + niveauSucre)
              println()
              println(laitInsuffisant) //rupture de stock donc retour au menu client
              println()
            }
            return false

          }  else { //Boisson réussie - Déduction des quantités utilisées du stock
            coffeeStocks(machineId) -= poudreLatteMoy
            milkStocks(machineId) -= laitLatteMoy
            prixBoisson = prixLatteMoyen
          }

        } else if (choixLatte == 3) {

          if (coffeeStocks(machineId)<12 || milkStocks(machineId) < 200) { //Vérification stock de café et lait pour au moins un latte petit
            //Réinitialisation des variables pour éviter que le programme continue au menu de lait supplémentaire.
            choixBoisson=0

            if (coffeeStocks(machineId) < 12) {
              println()
              println("Boisson sélectionnée : " + typeBoisson)
              println("Niveau de sucre : " + niveauSucre)
              println()
              println(poudreInsuffisante) //rupture de stock donc retour au menu client
            } else if (milkStocks(machineId)<200) {
              println()
              println("Boisson sélectionnée : " + typeBoisson)
              println("Niveau de sucre : " + niveauSucre)
              println()
              println(laitInsuffisant) //rupture de stock donc retour au menu client
            }

            return false

          } else {

            coffeeStocks(machineId) -= poudreLatteGra
            milkStocks(machineId) -= laitLatteGra
            prixBoisson = prixLatteGrand
          }
        } //Fin choix taille Latte


      } //FIN ifs préparation boissons

      //AJOUT DE LAIT EN SUPPLÉMENT
      //Mise à jour des doses de lait disponibles
      doseLaitDispo=milkStocks(machineId)/50
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

          while (nbDoseLait>3){
            println("Vous pouvez ajouter 3 doses de lait par boisson maximum !. Veuillez sélectionner une dose plus petite.")
            nbDoseLait = readLine("> ").toInt
          }
          while (nbDoseLait<0){
            println("Veuillez saisir une quantité valide.")
            nbDoseLait = readLine("> ").toInt
          }

          while (nbDoseLait>doseLaitDispo){ //Tant que la dose saisie est plus grand que les doses disponibles...
            println(laitSuppInsuffisant) //message indiquant qu'il n'y pas assez de lait disponible et qu'il faut choisir une autre dose ou réapprovisionner en mode Admin
            println("Doses de lait disponibles : "+nbDoseLait)
            nbDoseLait = readLine("> ").toInt
            println()

          }
          if (nbDoseLait==0) {
            prixLait = 0
            laitSupp = "Non"
            choixLait=2
          }else if (nbDoseLait>0){
            //Si la dose de lait choisie est plus petite que la dose dispo...le programme continue et fait la deduction du stock et le calcul du prix
            milkStocks(machineId) -= (doseLait * nbDoseLait)
            prixLait = prixDoseLait * nbDoseLait
            laitSupp = "Oui"
          }


        } else if (choixLait == 2) {
          laitSupp = "Non"
          prixLait=0
        }
      } else if ((choixBoisson == 2 || choixBoisson == 3) && doseLaitDispo<1){//Si pas assez de lait pour ajouter en supplément
        println()
        println("Boisson sélectionnée : " + typeBoisson)
        println("Niveau de sucre : " + niveauSucre)
        println("Pas assez de lait pour ajouter en supplément.")
        println()
        Thread.sleep(2000)
        laitSupp = "Non"
        choixLait=2
        prixLait=0
      } //Fin válidation lait en supp.

      //FIN CHOIX BOISSONS

      //ÉCRAN DE PAIEMENT*******

      if (true) {

        prixTotal = prixBoisson + prixSucre  //Addition du prix de la boisson, du sucre

        println("Boisson sélectionnée : " + typeBoisson)
        println("Niveau de sucre : " + niveauSucre)

        //Validation et calcul prix boissons et suppléments
        if ((choixBoisson == 2 || choixBoisson == 3) && (choixSucre == 1 && choixLait == 2)) { //Total pour Cappuccino et Latte sans sucre ni lait
          println("Lait supplémentaire : " + laitSupp)
          printf("Prix total : CHF %.2f", prixTotal)
          println()
        } else if ((choixBoisson == 2 || choixBoisson == 3) && (choixSucre != 1 && choixLait == 2)) { //Total pour Cappuccino et Latte sans lait mais avec du sucre
          println("Lait supplémentaire : " + laitSupp)
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixBoisson, prixSucre, prixTotal)
          println()
        } else if ((choixBoisson == 2 || choixBoisson == 3) && (choixSucre == 1 && choixLait == 1)) { //Total pour Cappuccino et Latte sans sucre mais avec du lait
          prixTotal+=prixLait //mise à jour du prix total (+supp)
          println("Lait supplémentaire : " + laitSupp)
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixBoisson, prixLait, prixTotal)
          println()
        } else if (choixBoisson == 1 && choixSucre == 1) { //Total pour expresso sans sucre
          printf("Prix total : CHF %.2f", prixTotal)
          println()
        } else if (choixBoisson == 1 && choixSucre != 1) { //Total expresso avec du sucre
          printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixBoisson, prixSucre, prixTotal)
          println()
        } else { //Total pour les boissons avec sucre et lait 6
          prixTotal+=prixLait //mise à jour du prix total (+supp)
          println("Lait supplémentaire : " + laitSupp)
          printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixBoisson, prixSucre, prixLait, prixTotal)
          println()
        }
        codeTwint = Random.alphanumeric take 5 mkString

        println("Veuillez payer en utilisant Twint. \nVotre code de paiement est : " + codeTwint)
        println("(En attente de validation de paiement...)")
        println()
        Thread.sleep(3000)
        println("Merci ! Votre paiement a été accepté.\nPréparation de votre boisson...")
        println()
        Thread.sleep(2000)
        println("Votre " + typeBoisson + " est prêt ! Bonne dégustation !")
        println()
        Thread.sleep(3000)

        /*Réinitialisation des variables
        choixLatte=0
        choixSucre=0
        choixLait=0
        choixMachine = 5
        choixMode=0*/

        //Fin Messages écran de paiement *******
      } else if (false){
        //Réinitialisation des variables
        choixLatte=0
        choixSucre=0
        choixLait=0
        choixMachine = 5
        choixMode=0
      }


    } while (choixBoisson==0)

    true

  } //FIN MÉTHODE SERVECLIENT

//PROGRAMME PRINCIPAL
  def main(args: Array[String]): Unit = {


    do { //Boucle principale Tant que l'utilisateur ne quitte le programme choixMode!=3
      println(txtNomSociete)
      println(txtChoixMode)
      println(txtClient)
      println(txtAdmin)
      println(txtQuitter)
      choixMode = readLine("> ").toInt

      //validation mode et écran d'accueil
      while (choixMode < 1 || choixMode > 3) { //tant que le choix est différent aux trois choix valides 1) Client 2) Admin 3) Quitter
        println(choixNonValide) // válidation non valide
        println()
        println(txtChoixMode)
        println(txtClient)
        println(txtAdmin)
        println(txtQuitter)
        choixMode = readLine("> ").toInt
        println()
      }
      //CHOIX DE LA MACHINE
      if (choixMode==1 || choixMode==2) {
        println(txtChoixMachine)
        choixMachine = readLine("> ").toInt
        //validation choix Machine
        while (choixMachine < 1 || choixMachine > 5) { //tant que le choix est différent aux choix valides
          println(choixNonValide) // válidation non valide
          println()
          println(txtChoixMachine)
          choixMachine = readLine("> ").toInt
          println()
        }
        choixMachine-=1
      }


        //DEBUT MODE CLIENT

        if (choixMode == 1) {

          var ServiceClient = serveClient(choixMachine, coffeStocks, sugarStocks, milkStocks)

          while (!ServiceClient) {
            println(txtChoixMachine)
            choixMachine = readLine("> ").toInt
            //validation choix Machine
            while (choixMachine < 1 && choixMachine > 5) { //tant que le choix est différent aux choix valides
              println(choixNonValide) // válidation non valide
              println()
              println(txtChoixMachine)
              choixMachine = readLine("> ").toInt
              println()
            }
            choixMachine-=1
            ServiceClient = serveClient(choixMachine,coffeStocks,sugarStocks, milkStocks)
          } //Fin boucle while ServiceClient faux

          //Réinitialisation des variables
          choixBoisson=0
          choixLatte=0
          choixSucre=0
          choixLait=0
        }

      //DEBUT MODE ADMIN


    else if (choixMode==2) {

      val validationAdmin = validatePin(choixMachine, machinePins)
         if (validationAdmin){
           println()
           println()
           println("Bienvenue au mode Admin !")
           println()
           println("Que souhaitez-vous faire ? ")
           println("1)Réapprovisionner les stocks\n2)Changer le code PIN de la machine")
           choixAdmin=readLine("> ").toInt
           println()
           while (choixAdmin<1 || choixAdmin>2){
             println(choixNonValide)
             choixAdmin=readLine("> ").toInt
           }

           if (choixAdmin==1){
             restockMachine(choixMachine, coffeStocks, sugarStocks, milkStocks)
           }else if (choixAdmin==2){
             updatePin(choixMachine,machinePins)
           }


         }

      }


    } while (choixMode!=3) //Tant que choixMode soit égale égale à 0 (valeur initiale) et différent à 3 (valeur qui sert à quitter le programme) la boucle s'exécutera.

    println("Merci ! Au revoir ! ")





  }
}