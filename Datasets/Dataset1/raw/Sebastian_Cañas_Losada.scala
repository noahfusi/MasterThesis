import io.StdIn._
import math._

object Main {
  def main(args: Array[String]): Unit = {
    //on définit les stockes initiaux et le code Admin
    var cafeStock = 50 //en grammes
    var sucreStock = 30 //en grammes
    var laitStock = 500 //en millilitres
    val codeAdmin = "434343" //code pin

    //menu principal
    var programme = true
    while(programme){
      println("        Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      val choix = readLine("> ").toInt
      println()

      if (choix == 1){
        println("Mode client")
        println()
        var modeClient = true
        var choixBoissonValable = false

        while(modeClient && !choixBoissonValable){
          //déclaration variable utile
          var prixBase = 0.0 //en CHF
          var cafeBoisson = 0 //en g
          var laitBoisson = 0 //en ml
          var doseLaitSupp = 50 //en ml
          var prixLaitSupp = 0.05 //CHF
          val prixLaitSuppArrondi = math.round(prixLaitSupp * 100.0) / 100.0
          var laitSupp = ""
          var sucreG = 0 //en g
          var prixSucre = 0.0 //en CHF
          var quantiteSucreS = "" //expression avec des mots
          var nomBoisson = ""
          var tailleBoisson = ""
          val AlphaNum = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" //caractère dispo pour le code Twint
          var codeTwint = ""

          while(!choixBoissonValable) {
            val choixBoisson = readLine("Veuillez sélectionner votre boisson : " +
              "\n1) Expresso - CHF 2.00" +
              "\n2) Cappuccino - CHF 2.50" +
              "\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)" +
              "\n> ")

            //développement des choix
            if (choixBoisson == "1") {
              nomBoisson = "Expresso"
              println(s"Vous avez sélectionné: $nomBoisson")
              choixBoissonValable = true
              println()

              //attribuer les valeurs du type de boisson aux variables utiles
              prixBase = 2.00
              cafeBoisson = 8
            }
            else if (choixBoisson == "2") {
              nomBoisson = "Cappuccino"
              println(s"Vous avez sélectionné: $nomBoisson")
              choixBoissonValable = true
              println()

              //attribuer les valeurs du type de boisson aux variables utiles
              prixBase = 2.5
              cafeBoisson = 6
              laitBoisson = 100
            }
            else if (choixBoisson == "3") {
              nomBoisson = "Latte"
              println(s"Vous avez sélectionné: $nomBoisson")
              var tailleLatteValable = false
              println()

              while (!tailleLatteValable) {
                println()
                val tailleLatte = readLine("Quelle taille désirez-vous ?" +
                  "\n1) Petit = 2.70 CHF" +
                  "\n2) Moyen = 3.20 CHF" +
                  "\n3) Grand = 3.70 CHF" +
                  "\n> ")

                if (tailleLatte == "1") {
                  tailleBoisson = "(Petit)"
                  prixBase = 2.5
                  cafeBoisson = 6
                  laitBoisson = 100
                  tailleLatteValable = true
                }
                else if (tailleLatte == "2") {
                  tailleBoisson = "(Moyen)"
                  prixBase = 2.5
                  cafeBoisson = 6
                  laitBoisson = 100
                  tailleLatteValable = true
                }
                else if (tailleLatte == "3") {
                  tailleBoisson = "(Grand)"
                  prixBase = 2.5
                  cafeBoisson = 6
                  laitBoisson = 100
                  tailleLatteValable = true
                }
                // si mauvaise entrée
                else {
                  println("Entrée invalide. Veuillez choisir un numéro entre 1 et 3.") //menu taille latte
                  println()
                }
              }
              //si taille latte valable
              choixBoissonValable = true
            }
            else {
              println("Entrée invalide. Veuillez choisir un numéro entre 1 et 3.") //menu choix boisson
              println()
            }
          }

          //boucle donnée valable quantité de sucre
          var choixSucreValable = false

          while (!choixSucreValable){
            println()
            val choixSucre = readLine("Souhaitez-vous ajouter du sucre ?" +
              "\n1) Sans sucre" +
              "\n2) Peu (5g) - CHF 0.10" +
              "\n3) Moyen (10g) - CHF 0.20" +
              "\n4) Beaucoup (15g) - CHF 0.30" +
              "\n> ")

            if(choixSucre == "1"){
              sucreG = 0
              quantiteSucreS = "Sans sucre"
              prixSucre = 0.0
              choixSucreValable = true
            }
            else if(choixSucre == "2"){
              sucreG = 5
              quantiteSucreS = "Peu"
              prixSucre = 0.1
              choixSucreValable = true
            }
            else if(choixSucre == "3"){
              sucreG = 10
              quantiteSucreS = "Moyen"
              prixSucre = 0.2
              choixSucreValable = true
            }
            else if(choixSucre == "4"){
              sucreG = 15
              quantiteSucreS = "Beaucoup"
              prixSucre = 0.3
              choixSucreValable = true
            }
            else{
              println("Entrée invalide. Veuillez choisir un numéro entre 1 et 3.") //menu choix quantité sucre
              Thread.sleep(2500)
              println()
            }
          }

          if(choixBoissonValable && (nomBoisson == "Latte" || nomBoisson == "Cappuccino")) {

            var choixLaitSupplementValable = false
            var doseLaitValable = false

            while (!choixLaitSupplementValable && !doseLaitValable) {
              println()
              val choixLait = readLine("Souhaitez-vous ajouter du lait en supplément ?" +
                "\n1) Oui" +
                "\n2) Non" +
                "\n> ")
              if(choixLait == "1") {

                while (!doseLaitValable){
                  println()

                  println("Vous pouvez ajouter jusqu'à 3 doses supplémentaire." +
                    "\nChacune contient 50 ml de lait et vaut 0.05 CHF.")

                  var doseLaitS = readLine("Combien de dose désirez-vous ?" +
                    "\n0) Aucune" +
                    "\n1) Une dose" +
                    "\n2) Deux doses" +
                    "\n3) Trois doses" +
                    "\n> ").toInt
                  println()

                  if(doseLaitS == 0 || doseLaitS == 1 || doseLaitS == 2|| doseLaitS == 3) {

                   doseLaitSupp *= doseLaitS
                   prixLaitSupp *= doseLaitS

                   println(s"$doseLaitS dose(s) de lait supplémentaire" +
                     s"\n-> $doseLaitSupp ml - CHF $prixLaitSuppArrondi")
                   laitSupp = "Oui"
                    doseLaitValable = true
                   println()
                  }
                  else {
                    println("Entrée invalide. Veuillez choisir un numéro entre 1 et 3.") //menu choix quantité sucre
                    Thread.sleep(2500)
                    println()
                  }
                }
              }
              else if (choixLait == "2"){
                println("Pas de lait en supplément")
                Thread.sleep(1500)
                println()
                laitSupp = "Non"
                choixLaitSupplementValable = true
                doseLaitValable = true
              }
              else{
                println("Entrée invalide. Veuillez choisir un numéro entre 1 et 2.") //menu choix quantité lait supplément
                Thread.sleep(2500)
                println()
              }
            }
          }

          //vérification de stock et variation

          if (cafeStock >= cafeBoisson && laitStock >= (laitBoisson + doseLaitSupp) && sucreStock >= sucreG) {

            //variation stock
            cafeStock -= cafeBoisson
            laitStock -= (laitBoisson + doseLaitSupp)
            sucreStock -= sucreG

            //génération du code pour paiement Twint
            for(_ <- 1 to 5){
              val randomIndex = (math.random() * AlphaNum.length).toInt //génère un index aléatoire compris dans alphanumerique
              val randomChar = AlphaNum(randomIndex) //sélectionne le caractère correspondant à l'index dans alphanumerique
              codeTwint += randomChar //ajoute ce caractère au code et le fait 5x
            }

            val prixTotal = prixBase + prixSucre + prixLaitSuppArrondi
            val prixTotalArrondi = math.round(prixTotal * 100.0) / 100.0

            if (nomBoisson == "Latte" || nomBoisson == "Cappuccino"){
              println(s"Boisson sélectionnée: $nomBoisson $tailleBoisson" +
                s"\nNiveau de sucre: $quantiteSucreS ($sucreG g)" +
                s"\nLait en supplément: $laitSupp" +
                s"\nPrix total: CHF $prixBase + CHF $prixSucre + CHF $prixLaitSuppArrondi = CHF $prixTotalArrondi")
              println()

              println("Veuillez payer en utilisant Twint." +
                s"\nVotre code de paiement est: $codeTwint " +
                "\n(En attente de validation du paiement...)")
              Thread.sleep(3000)
              println()
              println("Merci ! Votre paiement a été accepté.")
              println()
              Thread.sleep(3000)
              println("Préparation de votre boisson...")
              Thread.sleep(5000)
              println(s"Votre $nomBoisson est prêt! Bonne dégustation!")
              Thread.sleep(2000)
              println()
              modeClient = false
            }
            else {
              println(s"Boisson sélectionnée: $nomBoisson" +
                s"\nNiveau de sucre: $quantiteSucreS ($sucreG g)" +
                s"\nPrix total: CHF $prixBase + CHF $prixSucre + CHF $prixLaitSuppArrondi = CHF $prixTotalArrondi")

              println()

              println("Veuillez payer en utilisant Twint." +
                s"\nVotre code de paiement est: $codeTwint" +
                "\n(En attente de validation du paiement...)")
              Thread.sleep(3000)
              println()
              println("Merci ! Votre paiement a été accepté.")
              println()
              Thread.sleep(3000)
              println("Préparation de votre boisson...")
              Thread.sleep(5000)
              println(s"Votre $nomBoisson est prêt! Bonne dégustation!")
              Thread.sleep(2000)
              println()
              modeClient = false
            }
          }
          else if(cafeStock < cafeBoisson || laitStock < (laitBoisson + doseLaitSupp) || sucreStock < sucreG) {

            if (nomBoisson == "Latte" || nomBoisson == "Cappuccino"){
              if(laitStock < (laitBoisson + doseLaitSupp)){
                println(s"Boisson sélectionnée: $nomBoisson" +
                  s"\nNiveau de sucre: $quantiteSucreS ($sucreG g)" +
                  s"\nLait en supplément: $laitSupp")
                println()
                println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée." +
                  "\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
                println()
                modeClient = false
              }
              else if (cafeStock < cafeBoisson){
                println(s"Boisson sélectionnée: $nomBoisson" +
                  s"\nNiveau de sucre: $quantiteSucreS ($sucreG g)" +
                  s"\nLait en supplément: $laitSupp")
                println()
                println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée." +
                  "\nVeuillez choisir une autre boisson ou vérifier les stocks en Admin.")
                println()
                modeClient = false
              }
              else if (sucreStock < sucreG){
                println(s"Boisson sélectionnée: $nomBoisson" +
                  s"\nNiveau de sucre: $quantiteSucreS ($sucreG g)" +
                  s"\nLait en supplément: $laitSupp")
                println()
                println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée." +
                  "\nVeuillez choisir une dose de sucre plus petite.")
                println()
                modeClient = false
              }
            }
            else if(laitStock < (laitBoisson + doseLaitSupp)){
              println(s"Boisson sélectionnée: $nomBoisson" +
                s"\nNiveau de sucre: $quantiteSucreS ($sucreG g)")
              println()
              println("Erreur: Quantité de lait insuffisante pour préparer la boisson sélectionnée." +
                "\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
              println()
              modeClient = false
            }
            else if (cafeStock < cafeBoisson){
              println(s"Boisson sélectionnée: $nomBoisson" +
                s"\nNiveau de sucre: $quantiteSucreS ($sucreG g)")
              println()
              println("Erreur: Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée." +
                "\nVeuillez choisir une autre boisson ou vérifier les stocks en Admin.")
              println()
              modeClient = false
            }
            else if (sucreStock < sucreG){
              println(s"Boisson sélectionnée: $nomBoisson" +
                s"\nNiveau de sucre: $quantiteSucreS ($sucreG g)")
              println()
              println("Erreur: Quantité de sucre insuffisante pour préparer la boisson sélectionnée." +
                "\nVeuillez choisir une dose de sucre plus petite.")
              println()
              modeClient = false
            }
          }
        }
      }
      else if (choix == 2){
        println("Mode Admin")

        val tentativeMax = 3
        var tentative = 0
        var accesAutorise = false

        while (tentative < tentativeMax && !accesAutorise) {
          val codePIN = readLine("Entrez le code PIN: ")

          if (codePIN == codeAdmin){
            println("Accèes autorisé.")
            accesAutorise = true //condition pour gérer les stocks

            var retourMenuPrincipal = false //variable permettant le retour au menu principal

            while (accesAutorise && !retourMenuPrincipal){
              println()
              println("Stocks:")
              println(s"  Poudre de café: $cafeStock g")
              println(s"  Lait          : $laitStock ml")
              println(s"  Sucre         : $laitStock g")
              println()

              val varStock = readLine("Avez-vous ajouté du stock ?" +
                "\n1) Oui" +
                "\n2) Non" +
                "\n> ")
              println()

              if (varStock == "1"){
               val ajoutCafeStock = readLine("Combien de grammes de café avez-vous ajouté: ").toInt
               val ajoutSucreStock = readLine("Combien de grammes de sucre avez-vous ajouté: ").toInt
               val ajoutLaitStock = readLine("Combien de millitres de lait avez-vous ajouté: ").toInt
                cafeStock += ajoutCafeStock
                sucreStock += ajoutSucreStock
                laitStock += ajoutLaitStock

                println()
                println("Réapprovisionnement des stocks..." +
                  "\nAjout:" +
                  s"\n    Poudre de café: $ajoutCafeStock" +
                  s"\n    Lait          : $ajoutLaitStock" +
                  s"\n    Sucre         : $ajoutSucreStock" +
                  s"\nNiveaux de stock mis à jour." +
                  s"\nRetour au menu principal...")
               Thread.sleep(2000)
               retourMenuPrincipal =true
                println()
              }
              else if (varStock == "2"){
                println("Les stocks sont inchangés. " +
                  "\nRetour au menu principal ...")
                Thread.sleep(2000)
                println()
                retourMenuPrincipal = true//Quitte la boucle de vérification et renvoie directement au menu principal
              }
              else{
                println("Entrée invalide. Veuillez répondre par 1 ou 2.") //menu admin
              }
            }
          }
          else{
            tentative += 1
            println("Mot de passe érroné. Essayez à nouveau.")
            println()
          }
        }
        if (!accesAutorise){
          println("Mot de passe éronné. Retour au menu principal.")
          println()
        }
        //mode Admin
      }else if (choix == 3) {
        println("Merci d'avoir utilisé Nospresso !")
        programme = false //sortir du programme
        //quitter le programme
      }else {
        println("Entrée invalide, veuillez réessayer.") //menu principal
        println()
        //entrée non valide
      }
    }
  }
}