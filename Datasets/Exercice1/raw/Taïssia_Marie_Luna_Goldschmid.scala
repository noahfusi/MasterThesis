import scala.io.StdIn.readLine
import scala.util.Random
import scala.language.postfixOps
import scala.math.random
import scala.reflect.internal.NoPhase.assignsFields.||
import scala.reflect.internal.util.TriState.True
object Main {
  def main(args: Array[String]): Unit = {
    var cafe = 50;
    var sucre = 30;
    var lait = 0.500;
    var taille = "";
    var prix = 0.00;
    var prixsucre = 0.00;
    var prixlait = 0.00;
    var cmblait = "";
    var laitsupp = "";
    println("Nospresso Café \n Veuillez sélectionner votre mode: \n 1) Client \n 2) Admin \n 3) Quitter \n > ")
    var mode = readLine()
    while (mode != "3") {
      if (mode == "1") {
        println("Commande : \n Choisissez votre boisson : \n 1) Expresso - CHF 2.00 \n 2) Cappuccino - CHF 2.50 \n 3) Latte \n > ")
        var boisson = (readLine())
        if (boisson == "3") {
          println("Quel taille : \n 1) Petit - CHF 2.70 \n 2) Moyen - CHF 3.20 \n 3) Grand - CHF 3.70 \n >")
          taille = readLine()
          boisson = "Latte";
          if (taille == "1"){
            prix = prix + 2.70;
          }
          if (taille == "2"){
            prix = prix + 3.20;
          }
          if (taille == "3"){
            prix = prix + 3.70;
          }
        }
        println("Ajouter du sucre : \n 1) Pas de sucre \n 2) Peu de sucre (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - 0.30")
        var cmbsucre = readLine()
        if (boisson == "2" || taille == "1" || taille == "2" || taille == "3") {
          println("Souhaitez-vous du lait en supplément ? \n 1) Oui \n 2) Non \n >" )
          laitsupp = readLine()
          if (laitsupp == "2"){
            laitsupp = "Non";
          }
          if(laitsupp == "1") {
            laitsupp = "Oui";
            println("Ajouter du lait : \n 1) une dose (50ml) - CHF 0.05 \n 2) deux doses (100ml)- CHF 0.10 \n 3) trois doses (150ml) - CHF 0.15")
            cmblait = readLine()
          }
        }
        if (boisson == "1") {
          cafe = cafe - 8;
          prix = prix + 2.00;
          boisson = "Expresso";
        }
        if (boisson == "2") {
          cafe = cafe - 6;
          lait = lait - 0.100;
          prix = prix + 2.50;
          boisson = "Cappuccino";
        }
        if (boisson == "3" && taille == "1") {
          cafe = cafe - 6;
          lait = lait - 0.120;
          prix = prix + 2.70;
        }
        if (boisson == "3" && taille == "2") {
          cafe = cafe - 8;
          lait = lait - 0.150;
          prix = prix + 3.20;
        }
        if (boisson == "3" && taille == "3") {
          cafe = cafe - 12;
          lait = lait - 0.200;
          prix = prix + 3.70;
        }
        if (cmbsucre == "1") {
          sucre = sucre - 0;
          cmbsucre = "Pas de sucre";
        }
        if (cmbsucre == "2") {
          sucre = sucre - 5;
          prixsucre = prixsucre +0.10;
          cmbsucre = "Peu";
        }
        if (cmbsucre == "3") {
          sucre = sucre - 10;
          prixsucre = prixsucre +0.20;
          cmbsucre = "Moyen";
        }
        if (cmbsucre == "4") {
          sucre = sucre - 15;
          prixsucre = prixsucre +0.30;
          cmbsucre = "Beaucoup";
        }
        if(cmblait == "1") {
          lait = lait - 0.05;
          cmblait = "50ml";
          prixlait = prixlait + 0.05;
        }
        if(cmblait == "2") {
          lait = lait - 0.10;
          cmblait = "100ml";
          prixlait = prixlait + 0.10;
        }
        if(cmblait == "3") {
          lait = lait - 0.15;
          cmblait = "150ml";
          prixlait = prixlait + 0.15;
        }
        println("Paiement : \n")
        val twint = (math.random * (99999-0) + 0).toInt
        println("Boisson séléctionnée : " + boisson + "\n Niveau de sucre : " + cmbsucre )
        if (boisson == "2" || boisson == "3"){
          println("\n Lait supplémentaire : " + laitsupp)
        }
        if (lait == 0 || cafe == 0 || sucre == 0) {
          if (lait == 0) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson séléctionnée. \n Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          }
          if (cafe == 0) {
            println("Erreur : Quantité de cafe insuffisante pour préparer la boisson séléctionnée. \n Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          }
          if (sucre == 0) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson séléctionnée. \n Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          }
          System.exit(0)
        }
        var prixtotal = prix + prixsucre + prixlait;
        println("Prix total : CHF " + prix + " + CHF " + prixsucre + " + CHF " + prixlait + " = " + prixtotal)
        println("\n Veuillez payer en utilisant twint")
        println("Paiement via Twint, le code : "  + twint)
        println("(en attente de validation du payement...)")
        Thread.sleep(3000)
        println("Merci! votre payement a été accepté")
        println("Préparation de votre boisson...")
        println("[...]")
        if (boisson == "1"){
          println("Votre expresso est prêt! bonne dégustation!")
        }
        if (boisson == "2"){
          println("Votre cappuccino est prêt! bonne dégustation!")
        }
        if (boisson == "3"){
          println("Votre latte est prêt! bonne dégustation!")
        }
      }
      if (mode == "2") {
        println("Entrez le code PIN (434343): \n >")
        val code = readLine()
        if (code == "434343") {
          println("Niveau stock : \n Poudre de café : " + cafe + "g\n Sucre : " + sucre + "g\n Lait : " + lait + " litres")
          var addcafe = readLine().toInt
          var addsucre = readLine().toInt
          var addlait = readLine().toDouble
          cafe = cafe + addcafe
          lait = lait + addlait
          sucre = sucre + addsucre
          println("Niveau stock : \n Poudre de café : " + cafe + "g\n Sucre : " + sucre + "g\n Lait : " + lait + " litres")
        }
      }
      else {

      }
      Thread.sleep(5000)

      println("Nospresso Café \n Veuillez sélectionner votre mode: \n 1) Client \n 2) Admin \n 3) Quitter \n > ")
      mode = readLine()

    }
  }
}