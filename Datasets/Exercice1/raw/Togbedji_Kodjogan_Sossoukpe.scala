import scala.io.StdIn._
import math._
object Main {
  def main(args: Array[String]): Unit = {
    import scala.util.Random

    var stock_cafe: Double = 50
    var stock_lait = 0.5
    var stock_sucre = 30
    val codepin = 434343
    var peu_sucre = 5
    var moy_sucre = 10
    var beacoup_sucre = 15
    val dose_lait = 50
    val doubledoselait = 100
    val tripledoselait = 150 //l


    var quantisucre = 0
    var q_cafe = 0
    var q_lait = 0
    var taille_latte = 0
    var prix_expresso = 2.00
    val prix_cappu = 2.50
    val prix_petitlatte = 2.70
    var prix_moyenlatte = 3.20
    var prix_grandLatte = 3.70
    val prix_sucre = 0.02  //per gramme
    val prix_lait = 0.05// p ml?
    var prix_de_base = 0.0

    print("Nospresso Café...")
    println("Veuillez selectionner votre mode  :")
    println("1) Client \n 2) Admin \n 3) Quitter")
    var mode = readLine("> ").toInt

    // de l'entree de choix //Er
    while ((mode != 1)&& (mode != 2)&& (mode != 3)) { println("Entrez une valeur valide ! 1,2 ou 3 uniquement  :")
      println("1) Client \n2) Admin \n3) Quitter")
      mode = readLine(" > ").toInt}


    if(mode == 1){var boisson = 0
      println("Choix de votre boisson ?")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      boisson = readLine(">  ").toInt

      if (boisson ==1){prix_de_base = prix_expresso
        q_cafe = 8
        print("Souaitez-vous ajouter du sucre ?")
        println("1) Sans sucre ")
        print(" 2) Un peu (5g) - CHF 0.10 ")
        print("3) Moyen (10g) - CHF 0.20")
        println("4) Beaucoup (15g) - CHF 0.30")
        quantisucre = readLine("> ").toInt
        if (quantisucre == 1) quantisucre = 0
        else if (quantisucre == 2) quantisucre = peu_sucre
        else if (quantisucre == 3)quantisucre = moy_sucre
        else quantisucre= beacoup_sucre}

      if (boisson== 2){prix_de_base = prix_cappu
        q_cafe = 6
        q_lait = 100
        print("Souqitez-vous ajouter du sucre ?")
        println("1) Sans sucre \n2) Un peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20  \n4) Beaucoup (15g) - CHF 0.30")
        quantisucre= readLine("> ").toInt

        if(quantisucre == 1) {(quantisucre = 0)}
        else if(quantisucre == 2) {quantisucre = peu_sucre}
        else if (quantisucre == 3){quantisucre = moy_sucre}
        else quantisucre = beacoup_sucre
        println("Ajouter du laiten supplément  ? \n Disponible uniquement pour Cappucino et Latte")
        println("1) Non")
        println("2) Une dose")
        println("3) 2 doses")
        println("4) 3 doses")
        q_lait = readLine(">").toInt
        if (q_lait == 1) q_lait = 0
        else if (q_lait == 2) {q_lait += dose_lait}
        else if (q_lait == 3){q_lait += doubledoselait}
        else if (q_lait == 4)q_lait += tripledoselait
        else print("Mauvaise entree. Pas de lait supplementaire")}

      // choix Latte
      if(boisson == 3) {

        println("Choix de la la taille du Latte :")
        println("1) Petit \n 2) Moyen \n 3) Grand")
        taille_latte = readLine("> ").toInt
        if (taille_latte== 1){prix_de_base = prix_petitlatte
          q_cafe = 6
          q_lait = 120}
        else if(taille_latte == 2) {prix_de_base = prix_moyenlatte
        q_cafe = 8
          q_lait = 150}
        else {prix_de_base = prix_grandLatte
          q_cafe = 12
          q_lait = 200}
        println("Ajouter du sucre ?")
        println("1) Sans sucre \n 2) Un peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20\n  4) Beaucoup (15g) - CHF 0.30")
        quantisucre = readLine("> ").toInt
        if (quantisucre == 1) quantisucre = 0
     else if (quantisucre == 2) {
          quantisucre = peu_sucre}
        else if(quantisucre == 3) {quantisucre = moy_sucre}
        else quantisucre = beacoup_sucre

        print("Ajouter du lait Supplementaire? ")
        println ("1) Non \n 2) Une dose")
        println("3) 2 doses")
        println("4) 3  doses")
        q_lait = readLine(" > ").toInt
        if (q_lait == 1) q_lait = 0
        else if (q_lait == 2) {q_lait += dose_lait}
        else if (q_lait == 3) {q_lait += doubledoselait}
        else if (q_lait == 4) q_lait += tripledoselait
        else print("Mauvaise entree. Pas de lait supplementaire ajoute")
      }

      var codeTwint = ""
      if (stock_cafe < q_cafe){
        println("Erreur : Quantité de poudre de café insuffisante. \n Veuillez choisir une autre boisson ou verifier les stocks en mode Admin.")}
      else if (stock_sucre < quantisucre) {println("Erreur : Quantité de sucre insuffisante.")}
      else if(stock_lait < (q_lait / 1000.0)) {
        println ("Erreur : Quantité de lait insuffisante. Veuillez choisir une taille plus petite ou essayez une autre boisson")
      } else {stock_cafe -= q_cafe
        stock_sucre -= quantisucre
        stock_lait -= q_lait / 1000 //mettre en type double ptet

        println("Prix  de votre commande : " + (prix_de_base + (quantisucre * prix_sucre) + (q_lait * prix_lait)) + "CHF")
        println("Paiement par Twint uniquement. Nous allons generer le code du paiement ")
        print()
          codeTwint = (Random.alphanumeric.take(5).mkString.toUpperCase())
        print("Votre code de paiement est " + codeTwint )
        println("(En Attente de Paiement... )")
        Thread.sleep(3000)
        print()
        println("Merci. Paiement accepté! ")
        println("Préparation de votre boisson...")
        println("[...]")
        Thread.sleep(5000)

        println("Votre " + boisson + "est prêt !. Bonne degustation")}}

    else if (mode == 2) {var code = readLine("Votre code PIN : ").toInt

      while (code != codepin) {println("Code incorrect. Réessayez.")
        code = readLine("Votre PIN : ").toInt}
      print("Acces Autorisé.")
      print(s"Stocks: \n Poudre de Café  = $stock_cafe g \n Lait           = $stock_lait Litres \n Sucre          = $stock_sucre gramme")
      println()
      println("Reapprovisionnement des Stocks")
      println("1) Reapprovisionner\n2) quitter")
      val choixadmin = readLine(">").toInt

      if (choixadmin == 1) {var cafe_supp = readLine("Poudre de Cafe : ").toInt
        stock_cafe += cafe_supp
        var sucre_supp = readLine("Sucre :").toInt
        stock_sucre += sucre_supp
        var lait_supp = readLine("Lait :").toInt
        stock_lait += lait_supp
        println("Stocks mis à jour.")
        println("Retour au menu principal")}
      else if (choixadmin == 2) {println("Retour a l'acceuil")}
      else {print("mauvaise entree...")}}

    else {println(" Au Revoir ! ")}

  }
}