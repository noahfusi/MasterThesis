import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

object Main {
  def main(args: Array[String]): Unit = {
    var mot_de_passe_admin = 434343
    var stock_cafe = 50.0
    var stock_sucre = 30.0
    var stock_lait = 500.0
    var plus_cafe = 0.0
    var quantite_plus_sucre = 0.0
    var plus_lait = 0.0
    val plus_petit_lait = 50.0
    val plus_moyen_lait = 100.0
    val plus_grand_lait = 150.0
    val prix_expresso = 2.0
    val prix_capuccino = 2.5
    val prix_petit_latte = 2.7
    val prix_moyen_latte = 3.2
    val prix_grand_latte = 3.7
    val petit_sucre = 0.05
    val moyen_sucre = 0.10
    val grand_sucre = 0.15
    val supplement_petit_lait = 0.05
    val supplement_moyen_lait = 0.10
    val supplement_grand_lait = 0.15
    val supplement_petit_sucre = 0.10
    val supplement_moyen_sucre = 0.20
    val supplement_grand_sucre = 0.30
    var prix_total_lait = 0.0
    var quantite_plus_lait = 0.0
    //var recapitulatif_prix = ""
    val alphaNumeric = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var prix_boisson = 0.0
    var stock = 0.0
    var cafe_utilise = 0.0
    var prix_plus_sucre = 0.0
    var prix_plus_lait = 0.0
    var lait_utilise = 0.0
    var prix_total = 0.0
    var quantite_total_lait = 0.0
    var prix_latte = 0.0
    var forwhile = 0
    // demander a l'utilisateur quel mode il souhaite choisir......
    while (forwhile != 1) {
      var mode = readLine("bonjour \n tapez (1) si vouz-souhaitez boire une boisson\n tapez (2) si vous souhaitez voir/reaprovisionner les stocks .").toInt

      val lettre_aleatoire1 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
      val lettre_aleatoire2 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
      val lettre_aleatoire3 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
      val lettre_aleatoire4 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
      val lettre_aleatoire5 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
      val code_twint = lettre_aleatoire1 + lettre_aleatoire2 + lettre_aleatoire3 + lettre_aleatoire4 + lettre_aleatoire5

      while (mode == 1) {
        val boisson_choisi = readLine("Quelle boisson voulez-vous \n1)espresso \n2)Cappuccino \n3)Latte").toInt
        if (boisson_choisi == 1) {
          prix_boisson = prix_expresso
          cafe_utilise = 8.0
        }
        if (boisson_choisi == 2) {
          prix_boisson = prix_capuccino
          cafe_utilise = 8.0
          lait_utilise = 100.0
          println(" Souhaitez-vous ajouter du lait en supplement ?\n(Disponible uniquement pour Cappuccino et Latte)\n(1) Oui\n(2) Non")
          plus_lait = readInt()
          if (plus_lait == 2) {
            prix_plus_lait = 0.0
            quantite_plus_lait = 0.0
          }
          if (plus_lait == 1) {
            println("combien de dose voulez-vous ? ( jusqu'a 3 )")
            plus_lait = readInt()
            if (plus_lait == 1) {
              prix_plus_lait = supplement_petit_lait
              quantite_plus_lait += plus_petit_lait
              prix_total_lait = prix_plus_lait
            }
            if (plus_lait == 2) {
              prix_plus_lait = supplement_moyen_lait
              quantite_plus_lait += plus_moyen_lait
              prix_total_lait = prix_plus_lait
            }
            if (plus_lait == 3) {
              prix_plus_lait = supplement_grand_lait
              quantite_plus_lait += plus_grand_lait
              prix_total_lait = prix_plus_lait
            }
          }
        }
        if (boisson_choisi == 3) {
          println(" de quelle taille souhaitez-vous votre latte \n tapez (1) pour un petitn tapez (2) pour un moyen\n tapez (3) pour un grand")
          prix_latte = readInt()
          if (prix_latte == 1) {
            prix_boisson += prix_petit_latte
            cafe_utilise = 6.0
            lait_utilise = 120.0
          }
          if (prix_latte == 2) {
            prix_boisson += prix_moyen_latte
            cafe_utilise = 8.0
            lait_utilise = 150.0
          }
          if (prix_latte == 3) {
            prix_boisson += prix_grand_latte
            cafe_utilise = 12.0
            lait_utilise = 200.0
          }
          plus_lait = readLine(" voulez-vous ajoutez du lait: \n tapez (0) si non \n tapez (1) pour un petit peu de lait \n tapez (2) pour un peu de lait \n tapez (3) pour beaucoup de lait").toInt
          if (plus_lait == 0) {
            prix_plus_lait = 0.0
            quantite_plus_lait = 0.0
          }
          if (plus_lait == 1) {
            prix_plus_lait += supplement_petit_lait
            quantite_plus_lait += plus_petit_lait
            prix_total_lait += prix_plus_lait
          }
          if (plus_lait == 2) {
            prix_plus_lait += supplement_moyen_lait
            quantite_plus_lait += plus_moyen_lait
            prix_total_lait += prix_plus_lait
          }
          if (plus_lait == 3) {
            prix_plus_lait += supplement_grand_lait
            quantite_plus_lait += plus_grand_lait
            prix_total_lait += prix_plus_lait
          }
        }
        println(" voulez-vous ajoutez du sucre: \n tapez (0) si non \n tapez (1) pour un petit sucre \n tapez (2) pour un moyen sucre \n tapez (3) pour un grand sucre\n>")
        quantite_plus_sucre = readInt()
        if (quantite_plus_sucre == 0) {
          prix_plus_sucre = 0.0
          quantite_plus_sucre = 0.0
        }
        if (quantite_plus_sucre == 1) {
          prix_plus_sucre += supplement_petit_sucre
          quantite_plus_sucre = petit_sucre
        }
        if (quantite_plus_sucre == 2) {
          prix_plus_sucre += supplement_moyen_sucre
          quantite_plus_sucre = moyen_sucre
        }
        if (quantite_plus_sucre == 3) {
          prix_plus_sucre += supplement_grand_sucre
          quantite_plus_sucre = grand_sucre
        }
        plus_cafe = cafe_utilise
        quantite_total_lait = quantite_plus_lait + lait_utilise
        if (stock_cafe > plus_cafe && stock_sucre > quantite_plus_sucre && stock_lait > quantite_plus_lait) {
          prix_total = prix_boisson + prix_total_lait + prix_plus_sucre
          println("donc le prix total a payer  est : " + prix_total + " CHF ")
          println("paiement par twint\nvotre code pour le paiement est : " + code_twint)
          Thread.sleep(3000)
          println("le paiement a bien ete accepte \nPreparation de votre boisson...\n[...]\n Votre Cafe est pret ! Bonne d'egustation !")
          stock_cafe = stock_cafe - cafe_utilise
          stock_lait = stock_lait - quantite_plus_lait - lait_utilise
          stock_sucre = stock_sucre - quantite_plus_sucre
        }

        if (stock_cafe < plus_cafe || stock_sucre < quantite_plus_sucre || stock_lait < quantite_total_lait) {
          mode = 2
        }
      }
      while (mode == 2) {
        println(" quelle est le mot de passe :")
        mot_de_passe_admin = readInt()
        if (mot_de_passe_admin == 434343) {
          println(" voici les stock : Poudre de cafe: 10g\nLait : 0.5L\nSucre : 30g")
          println("tapez (1) pour reremplir les stocks\n tapez (2) pour retourner a l'espace client")
          stock = readInt()
          stock_sucre += 0.0
          stock_cafe += 50.0
          stock_lait += 500.0
          println("Reapprovisionnement des stocks...\nAjout :\nPoudre de caf ́e: 50\nLait : 500 ml\nSucre : 0\nNiveaux de stock mis `a jour.\nRetour au menu principal...")
          mode = 1
        }
      }
      if(mode ==3){
        forwhile +=1
        println("Merci! Aurevoir!")
      }
    }
  }
}