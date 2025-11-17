import scala.io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    //Stocks initiaux
    var stock_cafe: Int = 50
    var stock_lait: Double = 0.5
    var stock_sucre: Int = 30

    var Machine_ON: Boolean = true


    //Boucle principale
    while (Machine_ON) {
      println("       Nospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      var choixmenu = readLine("> ").toInt

      //Test Valeur correcte
      while (choixmenu<1 || choixmenu>3) {
        println("Veuillez fournir une entrée valide")
        choixmenu = readLine("> ").toInt
      }

      //QUITTER
      if (choixmenu==3) Machine_ON=false

      //MODE ADMIN
      else if (choixmenu==2){
        println("Mode Admin")
        var pin = readLine("Entrez le code PIN : ").toInt
        //Vérification PIN
        while (pin != 434343) {pin = readLine("PIN Incorrect\nEntrez le code PIN : ").toInt}

        println("Accès Autorisé\n\nStocks:")
        println("Poudre de café: " + stock_cafe +"g")
        println("Lait          : " + stock_lait + "L")
        println("Sucre         : " + stock_sucre + "g")
        println("")
        println("Réapprovisionnement des stocks...")
        println("Ajout:")
        //Ajout des quantités
        stock_cafe += readLine("Poudre de café: ").toInt
        stock_lait += readLine("Lait          : ").toDouble
        stock_sucre += readLine("Sucre         : ").toInt
        println("Niveaux de stock mis à jour.\nRetour au menu principal...\n")
      }
      //MODE CLIENT
      else if (choixmenu==1){

        println("Veuillez sélectionner votre boisson :")
        println("1) Expresso - CHF 2.00")
        println("2) Cappuccino - CHF 2.50")
        println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        var choixboisson= readLine("> ").toInt

        //Test Valeur correcte
        while (choixboisson<1 || choixboisson>3) {
          println("Veuillez fournir une entrée valide")
          choixboisson = readLine("> ").toInt}

        //Sélection de la taille de Latte
        var taille: Int = 0
        if (choixboisson==3){
          println("Veuillez sélectionner la taille")
          println("1) Petit - CHF 2.70")
          println("2) Moyen - CHF 3.20")
          println("3) Grand - CHF 3.70")
          taille += readLine("> ").toInt

          //Test Valeur correcte
          while (taille<1 || taille>3) {
            println("Veuillez fournir une entrée valide")
            taille = readLine("> ").toInt}}


        //Ajout de sucre
        println("Souhaitez-vous ajouter du sucre ?")
        println("1) Sans sucre")
        println("2) Peu (5g) - CHF 0.10")
        println("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        var addsucre = readLine("> ").toInt
        //Test Valeur correcte
        while (addsucre<1 || addsucre>4) {
          println("Veuillez fournir une entrée valide")
          addsucre = readLine("> ").toInt}


        //Ajout doses de lait
        var doses: Int = 0
        //(Condition => Cappuccino ou Latte)
        if (choixboisson==2 || choixboisson==3){
          println("Souhaitez-vous ajouter du lait en supplément ?")
          println("1) Oui")
          println("2) Non")
          var addmilk= readLine("> ").toInt
          //Test Valeur correcte
          while (addmilk<1 || addmilk>2) {
            println("Veuillez fournir une entrée valide")
            addmilk = readLine("> ").toInt}
          if (addmilk==1) {
            println("Combien de dose ?")
            doses = readLine("> ").toInt
            //Test Valeur correcte
            while (doses < 0 || doses > 3) {
              println("Veuillez fournir une entrée valide")
              doses = readLine("> ").toInt}
          }
        }
        //Test des quantités
        var quant_cafe: Int = 0
        var quant_lait: Double = 0
        var quant_sucre: Int = 0
        var prixboisson: Double = 0
        var prixsucre: Double= 0
        var prixlait: Double = 0


        //Test quantité et prix1
        if (choixboisson==1) {
          println("\nBoisson sélectionnée : Expresso")
          quant_cafe+=8
          prixboisson+=2.00}
        else if (choixboisson==2){
          println("\nBoisson sélectionnée : Cappuccino")
          quant_cafe+=6
          quant_lait+=0.1
          prixboisson+=2.50}
        else if (choixboisson==3){
          print("\nBoisson sélectionnée : Latte")
          if (taille==1){
            println(" (Petit)")
            quant_cafe+=6
            quant_lait+=0.12
            prixboisson+=2.70}
          else if (taille==2){
            println(" (Moyen)")
            quant_cafe+=8
            quant_lait+=0.15
            prixboisson+=3.20}
          else if (taille==3){
            println(" (Grand)")
            quant_cafe+=12
            quant_lait+=0.2
            prixboisson+=3.70}}

        print("Niveau de sucre : ")
        if (addsucre!=1 && addsucre!=0){
          if (addsucre==2) println("Peu (5g)")
          else if (addsucre==3) println("Moyen (10g)")
          else if (addsucre==4) println("Beaucoup (15g)")
          quant_sucre+= addsucre*5 - 5
          prixsucre += addsucre*0.1 - 0.1}
        else println("Sans Sucre")

        print("Lait en supplément: ")
        if (doses==0) {
          println("Non")}
        else {println("Oui, "+doses+" doses")
          quant_lait+=doses*0.05
          prixlait+=doses*0.05}

        if (quant_cafe<=stock_cafe){
          if (quant_lait<=stock_lait){
            if (quant_sucre<=stock_sucre){
              var total=prixboisson+prixlait+prixsucre
              printf("Prix total: CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n",prixboisson,prixlait,prixsucre,total)

              //Génération Code Twint
              val MDP_Gen = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
              val Twint_Code = Random.alphanumeric.filter(char => MDP_Gen.contains(char)).take(5).mkString

              println("\nVeuillez payer en utilisant Twint.")
              println("Votre code de paiement est : "+ Twint_Code)
              println("(En attente de validation du paiement...)")
              Thread.sleep(3000)
              println("\nMerci ! Votre paiement a été accepté.")
              stock_cafe-=quant_cafe
              stock_lait-=quant_lait
              stock_sucre=quant_sucre
              println("Préparation de votre boisson...\n[...]")
              Thread.sleep(5000)
              var boisson: String = ""
              if (choixboisson==1) boisson = "Expresso"
              else if (choixboisson==2) boisson = "Cappuccino"
              else boisson = "Latte"
              println("Votre "+boisson+" est prêt ! Bonne dégustation !\n\n")

            }
            else println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
          }
          else println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
        }
        else println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\n Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")

      }
    }
  }
}