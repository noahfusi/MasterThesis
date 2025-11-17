import scala.io.StdIn.{readInt, readLine}
import scala.util.Random
object Main {
  def main(args: Array[String]): Unit = {
    var fin = 0
    var choix = "0"
    var choixb = 0
    var sugar = 0
    var lait = 0
    var laitselection = "Non"
    var selection = "Cappucino"
    var taille = 0
    var price = 0.0
    var stocksugar = 30.0
    var stockcafe = 50.0
    var stocklait = 0.500
    val CODE = 464646
    var PIN = 0
    var dose = 0.0
    var x = 0
    def generateRandomCode(length: Int): String = {
      val alphanum = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      Random.alphanumeric.filter(alphanum.contains(_)).take(length).mkString
    }




    while (fin == 0) {
      while (choix !="1" && choix !="2" && choix !="3"){
        println()
        println("\nSelectionnez votre mode : :\n  1) Mode Client \n  2) Mode Admin\n  3) Quitter\n ")
        //possibilité d'afficher de mettre un msg d'erreur non demandé
        choix = readLine()
        if (choix == "3"){ fin = 1}
      }
      if (choix == "2")
        { while (PIN != CODE) {
          println("Code PIN ?")

          PIN = readInt()
        }
          if (PIN == CODE)
          {
            println("Accès autorisé.")
            println("Stock\n Poudre de café :" + stockcafe)
            println("Lait : " + stocklait )
            println("Sucre:  " + stocksugar)
            println("Réapprovisionnement des stocks...")
            println("de combien voulez vous réapprovisionner la machine en poudre de café (taper 0 si vous ne vouler par rajouter dans ce stock ")
            stockcafe = stockcafe + readInt()
            println("de combien voulez vous réapprovisionner la machine en Lait (taper 0 si vous ne vouler par rajouter dans ce stock ")
            stocklait = stocklait + readInt()
            println("de combien voulez vous réapprovisionner la machine en Sucre (taper 0 si vous ne vouler par rajouter dans ce stock ")
            stocksugar = stocksugar + readInt()
            println("Niveaux de stock mis à jour.")
            println("Retour au menu principal...")
            choix = "0"
            PIN = 0

          }


        }
      if (choix == "1") {
        while (choixb !=1 && choixb != 2 && choixb !=3){
          println("\nSelectionnez votre Boisson : :\n  1) Expresso - CHF 2.00 \n  2) Cappuccino - CHF 2.50\n  3) Latte CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n ")
          choixb = readInt()

        }
        if (choixb == 3) {
          println("Quelle taille ? \n 1) Latte CHF 2.70 (Petit) \n 2) CHF 3.20 (Moyen) \n 3) CHF 3.70 (Grand)" )
          taille = readInt()

        }

          println("\nSouhaitez-vous ajouter du sucre ?\n  1) Sans sucre \n  2) Peu (5g) - CHF 0.10\n  3) Moyen (10g) - CHF 0.20\n 4) Beaucoup (15g) - CHF 0.30\n ")

        sugar = readInt()
        var sugarlv = "Sans sucre"
        var sugarprice = 0.0
        var sugarS = 0
        if (sugar == 2) {
          sugarlv = "Peu (5g)"
          sugarprice = 0.10
          sugarS = 5


        }
        if (sugar == 3) {
          sugarlv = "Moyen (10g)"
          sugarprice = 0.20
          sugarS = 10

        }
        if (sugar == 4) {
          sugarlv = "Beaucoup (15g)"
          sugarprice = 0.30
          sugarS = 15

        }
          if (choixb == 2 || choixb == 3){
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("\n 1) Oui \n 2) Non \n")
            lait = readInt()
            if (lait == 1){
              println("Combien de dose ? 1 , 2 ou 3 ?")
              x = readInt()
              if (x==1){dose = 0.050}
              if (x==2){dose = 0.10}
              if (x==3){dose = 0.150}
              lait = 0
              laitselection = "Oui"

            }

          }
        if (choixb == 1 ){selection = "Expresso"
          price = 2.00

        }

          if (choixb == 2 ){selection = "Cappuccino"
        price = 2.50
          }
          if (choixb == 3 ){selection = "Latte"
            if (taille == 1){ price= 2.70}
            if (taille == 2){ price= 3.20}
            if (taille == 3){ price= 3.70}

          }

        val prixtot = price + sugarprice
        if(selection =="Latte") {
          if (taille == 1) {
            println("Boissons sélectionnée :Latte (petit)")
        }
          if (taille == 2) {
            println("Boissons sélectionnée :Latte (Moyen)")
          }
          if (taille == 3) {
            println("Boissons sélectionnée :Latte (Grand)")
          }
        }

        if(selection != "Latte"){println("Boissons sélectionnée :" + selection  )}
          println ("niveau de sucre :" + sugarlv)
          println("Lait supplémentaire :" + laitselection )

        if (choix=="1" ) {
          if(stocksugar < sugarS) {
          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ")
          choix = "0"
            choixb = 0

        }}
        if (choix == "2") {
          if(stocksugar < sugarS) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ")
            choix = "0"
            choixb = 0

          }
        }
        if (choix =="3" ) {
          if(stocksugar < sugarS) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ")
            choix = "0"
            choixb = 0

          }
        }



        if (choix=="1" && stockcafe < 8) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
          choix = "0"
          choixb = 0
        }
        if (choix == "2" && stockcafe < 6) {
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\"")
          choix = "0"
          choixb = 0
        }
        if (choix =="3" ) {
          if(taille == 1 && stockcafe < 6){
          println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\"")
          choix = "0"
            choixb = 0
        }
          if(taille == 2 && stockcafe < 8){
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\"")
            choix = "0"
            choixb = 0
          }
          if(taille == 3 && stockcafe < 12){
            println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\"")
            choix = "0"
            choixb = 0
          }
        }

        if (choix=="1" ) {
          if (stocklait < dose){
          println("Erreur Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
          choix = "0"
            choixb = 0
        }}
        if (choix == "2" ) {
          if (stocklait < (dose + 0.100)){
          println("Erreur Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
          choix = "0"
            choixb = 0
        }}
        if (choix =="3" ) {
          if(taille == 1 ){
            if (stocklait < (dose + 0.100)){
            println("Erreur Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
            choix = "0"
              choixb = 0
          }}
          if(taille == 2 ){
            if (stocklait < (dose + 0.150)){
              println("Erreur Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
              choix = "0"
              choixb = 0
            }}
          if(taille == 3 ){
            if (stocklait < dose + 0.200){
              println("Erreur Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
              choix = "0"
              choixb = 0
            }}
        }
        if (choix != "0") {

          println("Prix total : CHF " + price +" + " +sugarprice +" CHF = " + prixtot + " CHF "  )



        val paymentCode = generateRandomCode(5)
        println ("Veuillez payer en utilisant Twint.\n Votre code de paiement est : " + paymentCode)
          println( "En attente de validation du paiement...\n")
        Thread.sleep(4000)
          println("Merci ! Votre paiement a été accepté")
          choix = "0"
          if (choixb == 1){
            stockcafe = stockcafe - 8
          }
          if (choixb == 2){
          stockcafe = stockcafe - 6
            stocklait = stocklait - 0.100 - dose
        }
          if (choixb == 3){
            if (taille == 1){
              stockcafe = stockcafe - 6
              stocklait = stocklait - 0.100 - dose
            }
            if (taille == 2){
              stockcafe = stockcafe - 8
              stocklait = stocklait - 0.150 - dose
            }
            if (taille == 3){
              stockcafe = stockcafe - 12
              stocklait = stocklait - 0.200 - dose
            }
        }
        dose = 0
        taille = 0

        if (sugar != 1) {
          stocksugar = stocksugar - sugarS
          sugar = 0
        }
          println("Préparation de votre boisson...\n")
            println ("[...]")
          Thread.sleep(5000)
          println("Votre " + selection + " est prêt ! Bonne dégustation !")
          choixb = 0





        }}



      }
    }

}