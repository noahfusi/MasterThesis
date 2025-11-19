import scala.io.StdIn._
import scala.math._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    var Poudre_cafe = 50
    var Sucre = 30
    var Lait = 0.500
    var continuer = true

    //Menu principal
    while (continuer) {
      println("     Nospresso Café \nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
      val mode = readLine().toInt

      //Mode client
      if (mode == 1) {
        //Sélection de la boisson
        println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
        val boisson = readLine().toInt

        var prixBoisson = 0.0
        var poudre_cafe_requise = 0
        var lait_requis = 0.0

        if (boisson == 1) {
          prixBoisson = 2.00
          poudre_cafe_requise = 8
        }
        else if (boisson == 2) {
          prixBoisson = 2.50
          poudre_cafe_requise = 6
          lait_requis = 0.100
        }
        else if (boisson == 3) {
          println("Quelle taille désirez-vous ?\n1) Petit\n2) Moyen\n3) Grand ")
          val Latte_taille = readLine().toInt
          if (Latte_taille == 1) {
            prixBoisson = 2.70
            poudre_cafe_requise = 6
            lait_requis = 0.120
          }
          else if (Latte_taille == 2) {
            prixBoisson = 3.20
            poudre_cafe_requise = 8
            lait_requis = 0.150
          }
          else if (Latte_taille == 3) {
            prixBoisson = 3.70
            poudre_cafe_requise = 12
            lait_requis = 0.200
          }
        }

        //Sélection du lait pour cappuccino et latte

        else if (boisson == 2 || boisson == 3) {
          println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non")
          val lait = readLine().toInt
          var prix_dose_lait = 0.0
          var dose_lait_requis = 0.0
          if (lait == 1) {
            println("Combien de dose (50ml)? (3 doses max par boisson)")
            val dose_lait = readLine().toInt

            if (dose_lait > 0 && dose_lait <= 3){
              prix_dose_lait = dose_lait * 0.05
              dose_lait_requis = dose_lait * 0.050
            }
            else if (dose_lait > 3){
              println("Il n'y a que 3 doses maximales par boisson\nChoix invalide, retour au menu principal")
            }

          }
          else {
            println("Choix invalide, retour au menu principal")
          }
        }
        else {
          println("Choix invalide, retour au menu principal.")
        }

        //Sélection du sucre
        println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
        val sucre = readLine().toInt
        var prix_sucre = 0.0
        var sucre_requis = 0
        if (sucre == 1) {
          prix_sucre = 0.0
        }
        else if (sucre == 2) {
          prix_sucre = 0.10
          sucre_requis = 5
        }
        else if (sucre == 3) {
          prix_sucre = 0.20
          sucre_requis = 10
        }
        else if (sucre == 4) {
          prix_sucre = 0.30
          sucre_requis = 15
        }
        else {
          println("Choix invalide, retour au menu principal")
        }

        //Vérification des stocks
        val prix_Final = prixBoisson + prix_sucre
        if (Poudre_cafe >= poudre_cafe_requise && Lait >= lait_requis && Sucre >= sucre_requis) {
          println("Boisson sélectionnée : " + boisson)
          println("Niveau de sucre " + sucre)
          println("Lait supplémentaire : " + Lait)
          println("Prix total : CHF " + prix_Final)

          //Code TWINT
          val code_Twint = Random.alphanumeric.take(5).mkString
          println("Veuillez payer en utilisant Twint\nVotre code de paiement est : " + code_Twint)
          println("(En attente de validation du paiement...)")

          //Attente de 3 secondes pour simuler validation paiement
          Thread.sleep(3000)

          //Mise à jour des stocks
          Poudre_cafe -= poudre_cafe_requise
          Lait -= lait_requis
          Sucre -= sucre_requis

          println("Paiement confirmé.\nPréparation de votre boisson...")
          //Attente de 5 secondes pour préparation boisson
          Thread.sleep(5000)
          println("Votre " + boisson + " est prêt ! Bonne dégustation !")
        }
          else
          {
            println("Erreur : Stock insuffisant pour préparer la boisson sélectionnée.")
          }
        }

      //Mode admin
      else if (mode == 2) {
        println("Entrez le code PIN à 6 chiffres : ")
        val PIN = readLine().toInt
        if (PIN == 434343) {
          println("Accès autorisé")
          println("Stocks actuels :\nPoudre de café : " + Poudre_cafe + "\nSucre : " + Sucre + "\nLait : " + Lait)

          //Réapprovisionnement des stocks
          println("Réapprovisionnement des stocks...")
          println("Entrez la quantité de poudre de café à ajouter : ")
          val ajout_cafe = readLine().toInt
          println("Entrez la quantité de lait à ajouter (en litres) : ")
          val ajout_lait = readLine().toDouble
          println("Entrez la quantité de sucre à ajouter : ")
          val ajout_sucre = readLine().toInt

          //Mise à jour des stocks #2
          Poudre_cafe += ajout_cafe
          Lait += ajout_lait
          Sucre += ajout_sucre

          println("Niveaux des stocks mis à jour")
          println("Stocks actuels :\nPoudre de café :" + Poudre_cafe + "\nSucre :" + Sucre + "\nLait :" + Lait)
        }
        else {
          println("Code PIN incorrect")
        }
      }
      //Quitter
      else if (mode == 3) {
        println("Merci et à bientôt !")
        continuer = false
      }
      //Mode "erreur"
      else {
        println("Choix invalide, retour au menu principal")
      }
    }
  }
}
