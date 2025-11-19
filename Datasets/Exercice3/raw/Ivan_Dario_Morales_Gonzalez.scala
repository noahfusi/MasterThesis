import io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.language.postfixOps
import java.io.{FileWriter, PrintWriter}
import scala.io.Source._


object Main {

  //Déclarations
  //Valeurs et variables
  var machinesModifiees = new ArrayBuffer[Int]()
  var changementsAdmin = 0

  //MENU PRINCIPAL (saisie)
  //Machines
  var choixMachine: Int = 0
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
  val sucreInsuffisantDose= "Pas assez de sucre pour la dose sélectionnée. Veuillez choisir une autre machine, une dose plus petite ou en réapprovisionner en mode Admin"
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
  var codeSaisiAdmin=""
  var tentatives=3
  var ingredient=""
  var quantite=0
  var ravitailler=0


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

//CLASSE
  class Machine (id:Int, pincode:String, milk:Int, sugar:Int, coffee:Int){
    //Déclaration des attributs
    var idMachine=id
    var pinMachine = pincode
    var lait = milk
    var sucre = sugar
    var cafe = coffee


    //Déclaration des méthodes

    def addIngredient (ingredient:String,amount:Int): Unit = {
        if (ingredient=="c"){
         cafe+=amount
          Thread.sleep(1000)
          println("Café ravitaillé avec succès !")
          Thread.sleep(1000)
          println()
        }else if (ingredient=="s") {
          sucre+=amount

          println("Sucre ravitaillé avec succès !")

          println()
        } else if (ingredient=="l") {
          lait+=amount

          println("Lait ravitaillé avec succès !")

          println()
        }
      println()
      Thread.sleep(2000)


    }


    def removeIngredient (ingredient:String, amount:Int) : Boolean ={
      var rIngredient = ingredient
      var rQuantite=amount

        if (rIngredient=="café"){
          if (rQuantite>cafe){
            return false
          } else {
            cafe-=rQuantite

          }
        }else if (rIngredient=="sucre"){
         if (rQuantite>sucre) {
           return false
         } else {

           sucre-=rQuantite
         }
        } else if (rIngredient=="lait"){
          if (rQuantite>lait){
            return false
          }else {

            lait-=rQuantite
          }
        }
        true
    }

} //FIN CLASSE MACHINE

  //MÉTHODES ************************************************************

  def loadcsv(filename:String): ArrayBuffer[Machine] ={
    try {

    val fr = fromFile(filename)
    val machines = new ArrayBuffer[Machine]()

      val lignefr= fr.getLines()
      var i=0
      var IDMachine=1
      while (lignefr.nonEmpty){
        val ligne = lignefr.next
       if (i>0) {
          val colonne= ligne.split(",")
          machines += new Machine(id=IDMachine, pincode = colonne(0), milk = colonne(1).toInt, sugar = colonne(2).toInt, coffee = colonne(3).toInt
          )
         IDMachine+=1
       }
        i+=1
      }
      return machines
    }
    catch {
      case ex : java.io.FileNotFoundException => println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réssayez.")
        return null
    }

  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit ={
    var fichierCSV=filename
    fichierCSV="machines.csv"
    try {
      println("Sauvegarde des machines dans machines.csv...")
      println()
      val printWriter = new PrintWriter(new FileWriter(fichierCSV))
      printWriter.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        printWriter.println(machine.pinMachine + "," + machine.lait + "," + machine.sucre + "," + machine.cafe)
      }
     /* println("Sauvegarde de "+changementsAdmin+" machine(s) dans machines.csv...")
      println("Fichier sauvegardé avec succès.")
      machinesModifiees.clear()*/
      printWriter.close()
      Thread.sleep(2000)

    } catch {
      case ex : java.io.FileNotFoundException => println("Erreur : Échec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
        println("Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
        machinesModifiees.clear()
        changementsAdmin=0
        choixMode=3

    }

  }

  def enregistrerChangements(idMachine: Int): Unit = { //Méthode pour enregistrer les changements effectifs en mode admin
      if (!machinesModifiees.contains(idMachine)) {
        changementsAdmin += 1
        machinesModifiees += idMachine
      }
  }


  def validatePin(pincode:String, machines:ArrayBuffer[Machine]): Boolean={
    codeSaisiAdmin=pincode
    println("Machine sélectionnée (1-5) > "+(choixMachine+1)+".")
    codeSaisiAdmin=readLine("Entrez le code PIN > ")
    tentatives-=1
    while (codeSaisiAdmin!=machines(choixMachine).pinMachine && tentatives>0) {
      println("Code PIN incorrect. "+tentatives+ " tentatives restantes.")
      codeSaisiAdmin=readLine("> ")
      tentatives -=1
      if (tentatives==0 && codeSaisiAdmin!=machines(choixMachine).pinMachine){
        println()
        println()
        println("Trop de tentatives échouées. Fin du programme.")
        println()
        choixMode=3
      }
    }
    if (codeSaisiAdmin==machines(choixMachine).pinMachine) {
      println()
      println("Accès accordé.")
      Thread.sleep(1000)
      println()
      println("Chargement de la machine...")
      Thread.sleep(1000)
      println()
      tentatives=3 //Tentatives pour l'accès au mode admin réinitialisées
      true
    } else false

  }


  def updatePin(machines:ArrayBuffer[Machine]): Unit={

    println("Mise à jour du code PIN pour la Machine "+(choixMachine+1)+".")
    var nouveauCode=readLine(("Entrez un nouveau code PIN à 6 chiffres > "))
    val patron = "^[0-9]{6}$".r
    patron.matches(nouveauCode)
    while (!patron.matches(nouveauCode)){
      println("Le code doit être composé de 6 chiffres")
      nouveauCode = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }
    machines(choixMachine).pinMachine=nouveauCode
    println()
    savecsv("machines.csv",machines)
    if (choixMode!=3) {
      println("Le code PIN a été mis à jour avec succès !")
      Thread.sleep(2000)
      choixMode=2
    }
  }

  def choiceCheck(): Int={
    var choice = ""
    var choiceChecked=0
    while (choiceChecked==0){
      choice=readLine("> ")

      try {
        choiceChecked = choice.toInt
      } catch {
        case e:NumberFormatException => println("Veuillez saisir un chiffre. "+choice+" n'est pas un choix valide.")
      }
    }
    return choiceChecked
  }


  def serveClient(machines:ArrayBuffer[Machine]): Boolean={

    do {  //Menu client : choix de boissons


      println(txtNomSociete)
      println(txtChoixBoisson)
      println(txtExpresso)
      println(txtCappuccino)
      println(txtLatte)
      choixBoisson = choiceCheck()
      while (choixBoisson<1 || choixBoisson>3) {
        println(choixNonValide) // válidation mauvais choix
        println()
        println(txtChoixBoisson)
        println(txtExpresso)
        println(txtCappuccino)
        println(txtLatte)
        choixBoisson = choiceCheck()
        println()
      }
      //Attribution du type de boisson et deduction des quantités type de boisson
      var remove=true
      if (choixBoisson==1) {
        typeBoisson="Expresso"
        prixBoisson=prixExpresso
        remove=machines(choixMachine).removeIngredient("café",poudreExpresso)
        if (!remove){
          println(poudreInsuffisante)
          println("Boisson sélectionnée : " + typeBoisson)
          println()
          return false
        }
      } else if (choixBoisson==2) {
        typeBoisson="Cappuccino"
        prixBoisson=prixCapuccino
        remove=machines(choixMachine).removeIngredient("café",poudreCappuccino)
        if (!remove) {
          println(poudreInsuffisante)
          println("Boisson sélectionnée : " + typeBoisson)
          println()
          return false
        }
        remove=machines(choixMachine).removeIngredient("lait",laitCappuccino)
        if (!remove){
          println(laitInsuffisant)
          println("Boisson sélectionnée : " + typeBoisson)
          println()
          return false
        }

      } else if (choixBoisson==3) {
        println(txtChoixLatte)
        println(txtLattePetit)
        println(txtLatteMoyen)
        println(txtLatteGrand)
        choixLatte = choiceCheck()
        while (choixLatte<1 || choixLatte>3) { //Validation choix Latte
          println(choixNonValide) // válidation mauvais choix
          choixLatte = choiceCheck()
          println()
        }
        if (choixLatte==1) {
          typeBoisson=txtLattePetitfinal
          prixBoisson=prixLattePetit
          remove=machines(choixMachine).removeIngredient("café",poudreLattePet)
          if (!remove) {
            println(poudreInsuffisante)
            println("Boisson sélectionnée : " + typeBoisson)
            println()
            return false
          }
          remove=machines(choixMachine).removeIngredient("lait",laitLattePet)
          if (!remove){
            println(laitInsuffisant)
            println("Boisson sélectionnée : " + typeBoisson)
            println()
            return false
          }
        } else if (choixLatte==2) {
          typeBoisson=txtLatteMoyenfinal
          prixBoisson=prixLatteMoyen
          remove=machines(choixMachine).removeIngredient("café",poudreLatteMoy)
          if (!remove) {
            println(poudreInsuffisante)
            println("Boisson sélectionnée : " + typeBoisson)
            println()
            return false
          }
          remove=machines(choixMachine).removeIngredient("lait",laitLatteMoy)
          if (!remove){
            println(laitInsuffisant)
            println("Boisson sélectionnée : " + typeBoisson)
            println()
          }
        } else if (choixLatte==3) {
          typeBoisson=txtLatteGrandfinal
          prixBoisson=prixLatteGrand
          remove=machines(choixMachine).removeIngredient("café",poudreLatteGra)
          if (!remove) {
            println(poudreInsuffisante)
            println("Boisson sélectionnée : " + typeBoisson)
            println()
            return false
          }
          remove=machines(choixMachine).removeIngredient("lait",laitLatteGra)
          if (!remove){
            println(laitInsuffisant)
            println("Boisson sélectionnée : " + typeBoisson)
            println()
            return false
          }
        }
        }

      //AJOUT DU SUCRE
      if (true) {


        println(txtSucre) //Question
        println(txtSansSucre) //1
        println(txtPeuSucre) //2
        println(txtMoySucre) //3
        println(txtBcpSucre) //4
        choixSucre = choiceCheck()

        while (choixSucre < 1 || choixSucre > 4) {
          println(choixNonValide) // válidation mauvais choix
          choixSucre = choiceCheck()
          println()
        }

        //Attribution et deduction niveau de sucre
        if (choixSucre == 1) {
          niveauSucre = "Sans sucre"
        } else if (choixSucre == 2) {
          niveauSucre = "Peu"
          prixSucre = prixPeuSucre
          remove=machines(choixMachine).removeIngredient("sucre", peuSucre)
          if (!remove) {
            println(sucreInsuffisantDose)
            println("Quantité de sucre disponible : "+machines(choixMachine).sucre+"g")
            println()
            return false
          }
        } else if (choixSucre == 3) {
          niveauSucre = "Moyen"
          prixSucre = prixMoyenSucre
          remove=machines(choixMachine).removeIngredient("sucre", moySucre)
          if (!remove) {
            println(sucreInsuffisantDose)
            println("Quantité de sucre disponible : "+machines(choixMachine).sucre+"g")
            println()
            return false
          }
        } else if (choixSucre == 4) {
          niveauSucre = "Beaucoup"
          prixSucre = prixBcpSucre
          remove=machines(choixMachine).removeIngredient("sucre", bcpSucre)
          if (!remove) {
            println(sucreInsuffisantDose)
            println("Quantité de sucre disponible : "+machines(choixMachine).sucre+"g")
            println()
            return false
          }
        }
      } else if (false){
        //Réinitialisation des variables
        choixLatte=0
        choixSucre=0
        choixLait=0
        choixBoisson=0
        choixMachine=0
        choixMode=0
        prixTotal=0
        prixLait=0
        prixSucre=0
        prixBoisson=0
        niveauSucre=""
        typeBoisson=""
        laitSupp=""

      }

      if (true){
        //AJOUT DE LAIT EN SUPPLÉMENT
        //Mise à jour des doses de lait disponibles
        doseLaitDispo=machines(choixMachine).lait/50
        //Validation pour les boissons sur lesquelles on peut ajouter du lait en supp.
        if ((choixBoisson == 2 || choixBoisson == 3) && doseLaitDispo>=1) { //Si le client choisit un Cappuccino ou un Latte, le programme propose le lait supplémentaire
          println(txtLait)
          println("1)Oui")
          println("2)Non")
          choixLait = choiceCheck()

          while (choixLait<1 || choixLait>2) {
            println(choixNonValide) // válidation mauvais choix
            choixLait = choiceCheck()
            println()
          }

          if (choixLait == 1) { //Si le client veut ajouter du lait... message demandant la dose

            println(txtDoseLait)//combien de doses ?
            nbDoseLait = choiceCheck()

            while (nbDoseLait>3){
              println("Vous pouvez ajouter 3 doses de lait par boisson maximum !. Veuillez sélectionner une dose plus petite.")
              nbDoseLait = choiceCheck()
            }
            while (nbDoseLait<0){
              println("Veuillez saisir une quantité valide.")
              nbDoseLait = choiceCheck()
            }

            while (nbDoseLait>doseLaitDispo){ //Tant que la dose saisie est plus grand que les doses disponibles...
              println(laitSuppInsuffisant) //message indiquant qu'il n'y pas assez de lait disponible et qu'il faut choisir une autre dose ou réapprovisionner en mode Admin
              println("Doses de lait disponibles : "+doseLaitDispo)
              nbDoseLait = choiceCheck()
              println()

            }
            if (nbDoseLait==0) {
              prixLait = 0
              laitSupp = "Non"
              choixLait=2
            }else if (nbDoseLait>0){
              //Si la dose de lait choisie est plus petite que la dose dispo...le programme continue et fait la deduction du stock et le calcul du prix
              machines(choixMachine).removeIngredient("lait",doseLait * nbDoseLait)
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

      } else if (false){
        //Réinitialisation des variables
        choixLatte=0
        choixSucre=0
        choixLait=0
        choixMachine=0
        choixBoisson=0
        choixMode=0
        prixTotal=0
        prixLait=0
        prixSucre=0
        prixBoisson=0
        niveauSucre=""
        typeBoisson=""
        laitSupp=""

      }


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

        //Fin Messages écran de paiement *******

      } else if (false){
        //Réinitialisation des variables
        choixLatte=0
        choixSucre=0
        choixLait=0
        choixMachine=0
        choixBoisson=0
        choixMode=0
        prixTotal=0
        prixLait=0
        prixSucre=0
        prixBoisson=0
        niveauSucre=""
        typeBoisson=""
        laitSupp=""

      }


    } while (choixBoisson==0)

    true

  } //FIN MÉTHODE SERVECLIENT



  //PROGRAMME PRINCIPAL
  def main(args: Array[String]): Unit = {

    val machines = loadcsv("machines.csv")
    if (machines ==null) {
      println("Erreur : Échec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
      choixMode=3
    }

    while (choixMode!=3) { //Boucle principale Tant que l'utilisateur ne quitte le programme choixMode!=3


      println(txtNomSociete)
      println(txtChoixMode)
      println(txtClient)
      println(txtAdmin)
      println(txtQuitter)
      choixMode = choiceCheck()

      //validation mode et écran d'accueil
      while (choixMode < 1 || choixMode > 3) { //tant que le choix est différent aux trois choix valides 1) Client 2) Admin 3) Quitter
        println(choixNonValide) // válidation non valide
        println()
        println(txtChoixMode)
        println(txtClient)
        println(txtAdmin)
        println(txtQuitter)
        choixMode = choiceCheck()
        println()
      }
      //CHOIX DE LA MACHINE
      if (choixMode==1 || choixMode==2) {
        println(txtChoixMachine)
        choixMachine = choiceCheck()
        //validation choix Machine
        while (choixMachine < 1 || choixMachine > 5) { //tant que le choix est différent aux choix valides
          println(choixNonValide) // válidation non valide
          println()
          println(txtChoixMachine)
          choixMachine = choiceCheck()
          println()
        }
        choixMachine-=1
      }

      //DEBUT MODE CLIENT

      if (choixMode == 1) {

        println("Chargement des machines depuis machines.csv...\nMachine "+machines(choixMachine).idMachine+" chargée : \n  ID: "+machines(choixMachine).idMachine +"\n  Code PIN: "+machines(choixMachine).pinMachine+ "\n  Lait: "+machines(choixMachine).lait+"L\n  Sucre: "+machines(choixMachine).sucre+"g\n  Café: "+machines(choixMachine).cafe+"g")
        println()
        println("1 machine(s) chargée(s) avec succès.")
        println()
        println()
        var ServiceClient = serveClient(machines)

        while (!ServiceClient) {
          println(txtChoixMachine)
          choixMachine = choiceCheck()
          //validation choix Machine
          while (choixMachine < 1 && choixMachine > 5) { //tant que le choix est différent aux choix valides
            println(choixNonValide) // válidation non valide
            println()
            println(txtChoixMachine)
            choixMachine = choiceCheck()
            println()
          }
          ServiceClient = serveClient(machines)
          if (ServiceClient){

            savecsv("machines.csv",machines)
            println()
            Thread.sleep(2000)


          }

        } //Fin boucle while ServiceClient faux

        if (ServiceClient){

          savecsv("machines.csv",machines)
          println()
          Thread.sleep(2000)

        }

        //Réinitialisation des variables
        choixBoisson=0
        choixLatte=0
        choixSucre=0
        choixLait=0
      }

      //DEBUT MODE ADMIN


      else if (choixMode==2) {
        machinesModifiees.clear()
        changementsAdmin=0
        var validationAdmin = validatePin(machines(choixMachine).pinMachine, machines)



          if (validationAdmin) {

            while(choixMode==2){
              ravitailler=0//Réinitialisation de la variable
            println()
            println()
            println("Bienvenue au mode Admin !")
            println()
            println("Chargement des machines depuis machines.csv...\nMachine " + machines(choixMachine).idMachine + " chargée : \n  ID: " + machines(choixMachine).idMachine + "\n  Code PIN: " + machines(choixMachine).pinMachine + "\n  Lait: " + machines(choixMachine).lait + "L\n  Sucre: " + machines(choixMachine).sucre + "g\n  Café: " + machines(choixMachine).cafe + "g")
            println()
            println("1 machine(s) chargée(s) avec succès.")
            println()
            println("Que souhaitez-vous faire ? ")
            println("1)Ravitailler les stocks\n2)Changer le code PIN de la machine\n3)Changer de machine\n4)Retourner au menu principal")
            choixAdmin = choiceCheck()
            println()
            while (choixAdmin < 1 || choixAdmin > 4) {
              println(choixNonValide)
              choixAdmin = choiceCheck()
            }

            if (choixAdmin == 1) { //Ravitailler - AddIngredient
              ingredient = readLine("Quel est l'ingrédient à réapprovisionner ? c: café | s: sucre | l: lait) : >")
              while (ingredient != "c" && ingredient != "s" && ingredient != "l") {
                println("Veuillez saisir une entrée valide")
                ingredient = readLine("> ")
              }
              if (ingredient=="c"){
                ingredient="café"
              } else if (ingredient=="s") {
                ingredient = "sucre"
              } else if (ingredient=="l") {
                ingredient = "lait"
              }
              println("Quelle quantité de " + ingredient + " voulez-vous rajouter ?")
              quantite = choiceCheck()
              while (quantite < 0) {
                println("Veuillez saisir une quantité valide")
                quantite = choiceCheck()
              }
              machines(choixMachine).addIngredient(ingredient,quantite)
              enregistrerChangements(choixMachine + 1)
              //Demande si l'admin veut ravitailler un autre ingrédient
              while (ravitailler != 2) {
                  println("Que souhaitez-vous faire ? ")
                  println("1)Ravitailler un autre ingrédient\n2)Retourner au menu Admin")
                  ravitailler = choiceCheck()
                  println()
                  while ( ravitailler < 1 || ravitailler > 2) {
                    println(choixNonValide)
                    ravitailler = choiceCheck()
                  }

                    if (ravitailler == 1) { //Ravitailler un autre ingrédient - addIngredient
                      ingredient = readLine("Quel est l'ingrédient à réapprovisionner ? c: café | s: sucre | l: lait) : >")

                      while (ingredient != "c" && ingredient != "s" && ingredient != "l") {
                        println("Veuillez saisir une entrée valide")
                        ingredient = readLine("> ")
                      }
                      if (ingredient=="c"){
                        ingredient="café"
                      } else if (ingredient=="s") {
                        ingredient = "sucre"
                      } else if (ingredient=="l") {
                        ingredient = "lait"
                      }
                      println("Quelle quantité de " + ingredient + " voulez-vous rajouter ?")
                      quantite = choiceCheck()
                      while (quantite < 0) {
                        println("Veuillez saisir une quantité valide")
                        quantite = choiceCheck()
                      }
                      machines(choixMachine).addIngredient(ingredient,quantite)
                      enregistrerChangements(choixMachine + 1)
                    } else if (ravitailler == 2) { //Retour au menu admin
                      savecsv("machines.csv", machines) //Sauvegarde lors de modification de stocks
                      if (choixMode != 3) {
                        enregistrerChangements(choixMachine + 1)
                      }
                    }
              }
            } else if (choixAdmin == 2) { //Update Pin
              updatePin(machines)
              if(choixMode!=3){
                enregistrerChangements(choixMachine + 1)
              }

            } else if (choixAdmin == 3) {
              println(txtChoixMachine)
              choixMachine = choiceCheck()
              //validation choix Machine
              while (choixMachine < 1 || choixMachine > 5) { //tant que le choix est différent aux choix valides
                println(choixNonValide) // válidation non valide
                println()
                println(txtChoixMachine)
                choixMachine = choiceCheck()
                println()
              }
              choixMachine -= 1
              validationAdmin = validatePin(machines(choixMachine).pinMachine, machines)
              choixMode = 2
              choixAdmin=0
              ravitailler=0

            } else if (choixAdmin == 4) { //Retour au menu principal
              println()
              println("Sauvegarde de " + changementsAdmin + " machines dans machines.csv...")
              Thread.sleep(1000)
              println("Fichier sauvegardé avec succès.")
              println()
              Thread.sleep(2000)
              choixMode = 0
              choixMachine = 0
              choixAdmin=0
              ravitailler=0
            }


          }

          }



      }



    } //Tant que choixMode soit égale égale à 0 (valeur initiale) et différent à 3 (valeur qui sert à quitter le programme) la boucle s'exécutera.
    println()
    println("Merci ! Au revoir ! ")





  }
}