import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

object Main {
  def main(args: Array[String]): Unit = {
    var code_admin = 434343
    var cafe_stock = 50.0
    var sucre_stock = 30.0
    var lait_stock = 500.0
    var ajout_cafe = 0.0
    var sucre_quantite_ajout = 0.0
    var ajout_lait = 0.0
    val ajout_petit_lait = 50.0
    val ajout_moyen_lait = 100.0
    val ajout_grand_lait = 150.0
    val expresso_prix = 2.0
    val capuccino_prix = 2.5
    val petit_latte_prix = 2.7
    val moyen_latte_prix = 3.2
    val grand_latte_prix = 3.7
    val sucre_petit = 0.05
    val sucre_moyen = 0.10
    val sucre_grand = 0.15
    val extra_petit_lait = 0.05
    val extra_moyen_lait = 0.10
    val extra_grand_lait = 0.15
    val extra_petit_sucre = 0.10
    val extra_moyen_sucre = 0.20
    val extra_grand_sucre = 0.30
    var lait_prix_total = 0.0
    var lait_quantite_ajout = 0.0
    //var recapitulatif_prix = ""
    val alphaNumeric = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var boisson_prix = 0.0
    var stock_quantite = 0.0
    var cafe_utilise = 0.0
    var prix_ajout_sucre = 0.0
    var prix_ajout_lait = 0.0
    var lait_utilise = 0.0
    var prix_total = 0.0
    var quantite_tot_lait = 0.0
    var latte_prix = 0.0
    var forwhile = 0
    // demander a l'utilisateur quel mode il souhaite choisir......
    while (forwhile != 1) {
      var mode = readLine("bonjour \n Appuyez (1) si vouz-souhaitez prendre une boisson\n tapez (2) si vous souhaitez examiner/reaprovisionner les stocks .").toInt

      val lettre_hasard1 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
      val lettre_hasard2 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
      val lettre_hasard3 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
      val lettre_hasard4 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
      val lettre_hasard5 = alphaNumeric((math.random * alphaNumeric.length).toInt).toString
      val code_twint = lettre_hasard1 + lettre_hasard2 + lettre_hasard3 + lettre_hasard4 + lettre_hasard5

      while (mode == 1) {
        val boisson_selectionne = readLine("Quelle boisson souhaitez-vous \n1)espresso \n2)Cappuccino \n3)Latte").toInt
        if (boisson_selectionne == 1) {
          boisson_prix = expresso_prix
          cafe_utilise = 8.0
        }
        if (boisson_selectionne == 2) {
          boisson_prix = capuccino_prix
          cafe_utilise = 8.0
          lait_utilise = 100.0
          println(" Voulez-vous ajouter du lait en supplement ?\n(Disponible uniquement pour Cappuccino et Latte)\n(1) Oui\n(2) Non")
          ajout_lait = readInt()
          if (ajout_lait == 2) {
            prix_ajout_lait = 0.0
            lait_quantite_ajout = 0.0
          }
          if (ajout_lait == 1) {
            println("combien de portion voulez-vous ? ( jusqu'a 3 )")
            ajout_lait = readInt()
            if (ajout_lait == 1) {
              prix_ajout_lait = extra_petit_lait
              lait_quantite_ajout += ajout_petit_lait
              lait_prix_total = prix_ajout_lait
            }
            if (ajout_lait == 2) {
              prix_ajout_lait = extra_moyen_lait
              lait_quantite_ajout += ajout_moyen_lait
              lait_prix_total = prix_ajout_lait
            }
            if (ajout_lait == 3) {
              prix_ajout_lait = extra_grand_lait
              lait_quantite_ajout += ajout_grand_lait
              lait_prix_total = prix_ajout_lait
            }
          }
        }
        if (boisson_selectionne == 3) {
          println(" de quelle taille voulez-vous votre latte \n appuyez (1) pour un petitn appuyez (2) pour un moyen\n appuyez (3) pour un grand")
          latte_prix = readInt()
          if (latte_prix == 1) {
            boisson_prix += petit_latte_prix
            cafe_utilise = 6.0
            lait_utilise = 120.0
          }
          if (latte_prix == 2) {
            boisson_prix += moyen_latte_prix
            cafe_utilise = 8.0
            lait_utilise = 150.0
          }
          if (latte_prix == 3) {
            boisson_prix += grand_latte_prix
            cafe_utilise = 12.0
            lait_utilise = 200.0
          }
          ajout_lait = readLine(" voulez-vous ajoutez du lait: \n appuyez (0) si non \n appuyez (1) pour un petit peu de lait \n appuyez (2) pour un peu de lait \n appuyez (3) pour beaucoup de lait").toInt
          if (ajout_lait == 0) {
            prix_ajout_lait = 0.0
            lait_quantite_ajout = 0.0
          }
          if (ajout_lait == 1) {
            prix_ajout_lait += extra_petit_lait
            lait_quantite_ajout += ajout_petit_lait
            lait_prix_total += prix_ajout_lait
          }
          if (ajout_lait == 2) {
            prix_ajout_lait += extra_moyen_lait
            lait_quantite_ajout += ajout_moyen_lait
            lait_prix_total += prix_ajout_lait
          }
          if (ajout_lait == 3) {
            prix_ajout_lait += extra_grand_lait
            lait_quantite_ajout += ajout_grand_lait
            lait_prix_total += prix_ajout_lait
          }
        }
        println(" voulez-vous ajoutez du sucre: \n appuyez (0) si non \n appuyez (1) pour un petit sucre \n appuyez (2) pour un moyen sucre \n appuyez (3) pour un grand sucre\n>")
        sucre_quantite_ajout = readInt()
        if (sucre_quantite_ajout == 0) {
          prix_ajout_sucre = 0.0
          sucre_quantite_ajout = 0.0
        }
        if (sucre_quantite_ajout == 1) {
          prix_ajout_sucre += extra_petit_sucre
          sucre_quantite_ajout = sucre_petit
        }
        if (sucre_quantite_ajout == 2) {
          prix_ajout_sucre += extra_moyen_sucre
          sucre_quantite_ajout = sucre_moyen
        }
        if (sucre_quantite_ajout == 3) {
          prix_ajout_sucre += extra_grand_sucre
          sucre_quantite_ajout = sucre_grand
        }
        ajout_cafe = cafe_utilise
        quantite_tot_lait = lait_quantite_ajout + lait_utilise
        if (cafe_stock > ajout_cafe && sucre_stock > sucre_quantite_ajout && lait_stock > lait_quantite_ajout) {
          prix_total = boisson_prix + lait_prix_total + prix_ajout_sucre
          println("donc le prix total a payer  est : " + prix_total + " CHF ")
          println("paiement par twint\nvotre code pour le paiement est : " + code_twint)
          Thread.sleep(3000)
          println("le paiement a bien ete accepte \nPreparation de votre boisson...\n[...]\n Votre Cafe est pret ! Bonne dégustation !")
          cafe_stock = cafe_stock - cafe_utilise
          lait_stock = lait_stock - lait_quantite_ajout - lait_utilise
          sucre_stock = sucre_stock - sucre_quantite_ajout
        }

        if (cafe_stock < ajout_cafe || sucre_stock < sucre_quantite_ajout || lait_stock < quantite_tot_lait) {
          mode = 2
        }
      }
      while (mode == 2) {
        println(" quelle est le mot de passe :")
        code_admin = readInt()
        if (code_admin == 434343) {
          println(" voici les stock : Poudre de cafe: 10g\nLait : 0.5L\nSucre : 30g")
          println("appuyez (1) pour remplir à nouveau les stocks\n appuyez (2) pour retourner a l'espace client")
          stock_quantite = readInt()
          sucre_stock += 0.0
          cafe_stock += 50.0
          lait_stock += 500.0
          println("Reapprovisionnement des stocks...\nAjout :\nPoudre de caf ́e: 50\nLait : 500 ml\nSucre : 0\nNiveaux de stock mis `a jour.\nRetour au menu principal...")
          mode = 1
        }
      }
      if(mode ==3){
        forwhile +=1
        println("Merci! A bientot!")
      }
    }
  }


}