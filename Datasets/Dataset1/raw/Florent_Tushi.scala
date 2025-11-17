import scala.io.StdIn.readLine
object mainfinal {
  def main(args: Array[String]): Unit = {
    var mode = 0
    //déclarer les variables des stocks
    var stockcafe = 50.0
    var stocksucre = 30.0
    var stocklait = 0.5
    //déclarer les variable des prix
    var prixsucre = 0.0
    var prixlait = 0.0
    var prixcafe = 0.0
    var prixtotal= 0.0
    //déclarer les variable de choix de la boisson
    var boisson = 0
    var latte = 0
    //déclarer la valeur du code PIN en string
    val pin = "434343"
    //Déclarer les variables du choix de la dose des ingrédients
    var sucre = 0
    var lait = 0
    //déclarer la variable du choix sin on ajouter du lait supplémentaire ou non
    var laitoui = 0
    //déclarer les variables de la quantité nécessaire de chaque ingrédient
    var cafeutil = 0.0
    var sucreutil = 0.0
    var laitutil = 0.0
    //Déclarer la variable qui permet de creer la boucle et d'en sortir
    var dispo = false
    //déclarer les variables des doses d'ingrédient qu'on ajoute dans le mode admin
    var pluscafe = 0.0
    var plussucre = 0.0
    var pluslait = 0.0
    //déclarer une variable en string pour le nom de la boisson en fonction de la boisson choisie
    var nomboisson= "a"
     do {
       mode = readLine("Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toInt
       while (mode != 1 && mode != 2 && mode != 3) {
         mode = readLine("Veuillez taper une valeur entre 1 et 3 !\nRéessayez : Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toInt
       }
       if (mode == 1) {
         do {//boucle infinie si les stocks sont pas suffisant le programme revient tjr sur le mode client
           //Choix de la boisson
           println("\nMode client : Sélection de boisson\n")
           boisson = readLine("Veuillez sélectionner votre boisson\n1) Expresso - CHF 2.00\n2) Capuccino - CHF 2.50\n3) Latte - CHF 2.70 (petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
           while (boisson != 1 && boisson != 2 && boisson != 3) {
             boisson = readLine("Erreur tapez un chiffre entre 1 et 3.Veuillez sélectionner votre boisson\n1) Expresso - CHF 2.00\n2) Capuccino - CHF 2.50\n3) Latte - CHF 2.70 (petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) >").toInt
           }
           //fixer le prix et les ingrédients utilisé pour la boisson sélectionner
           if (boisson == 1) {
             nomboisson = "Expresso"
             cafeutil = 8
             prixcafe = 2.00
           }
           else if (boisson == 2) {
             nomboisson = "Cappuccino"
             cafeutil = 6
             laitutil = 0.1
             prixcafe = 2.50
           }
           else if (boisson == 3) {
             latte = readLine("Veuillez choisir la taille:\n1) Petit\n2) Moyen\n3) Grand\n>").toInt
             while (latte != 1 && latte != 2 && latte != 3) {
               println("Veuillez entrer une valeur entre 1 et 3 ! \nréessayez")
               latte = readLine("Veuillez choisir la taille:\n1) Petit\n2) Moyen\n3) Grand\n>").toInt
             }
             if (latte == 1) {
               nomboisson = "Latte (Petit)"
               cafeutil = 6
               laitutil = 0.12
               prixcafe = 2.70
             }
             if (latte == 2) {
               nomboisson = "Latte (Moyen)"
               cafeutil = 8
               laitutil = 0.15
               prixcafe = 3.20
             }
             if (latte == 3) {
               nomboisson = "Latte (Grand)"
               cafeutil = 12
               laitutil = 0.2
               prixcafe = 3.70
             }
           }
           //Personnalisation de la boisson
           sucre = readLine("Veuillez choisir la dose de sucre\n1) Pas de sucre\n2) Peu de sucre(5g)\n3) Moyen sucre(10g)\n4) Beaucoup de sucre(15g)\n>").toInt
           while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) {
             sucre = readLine("Veuillez taper une valeur entre 1 et 4 !\nRéessayez:\nVeuillez choisir la dose de sucre\n1) Pas de sucre\n2) Peu de sucre(5g)\n3) Moyen sucre(10g)\n4) Beaucoup de sucre(15g)\n>").toInt
           }
           //fixer le prix du sucre et le sucre utilisé
           if (sucre == 1) {
             prixsucre = 0
           }
           if (sucre == 2) {
             prixsucre = 0.10
             sucreutil = 5
           }
           if (sucre == 3) {
             prixsucre = 0.20
             sucreutil = 10
           }

           if (sucre == 4) {
             prixsucre = 0.30
             sucreutil = 15
           }
           var laitsupputil=0.0
           if (boisson == 2 || boisson == 3) {
             //demander si le client du lait supplémentaire seulement si il prend un capuccino ou un latte
             laitoui = readLine("Souhaitez vous ajouter du lait supplémentaire ? \n1) Oui\n2) Non\n>").toInt
             while (laitoui != 1 && laitoui != 2) {
               laitoui = readLine("Veuillez taper une valeur entre 1 et 2 !\nRéessayez.\nSouhaitez vous ajouter du lait supplémentaire ? \n1) Oui\n2) Non\n>").toInt
             }
             if (laitoui == 1) {
               lait = readLine("Combien de doses ? (max. 3)\n>").toInt
               while (lait != 1 && lait != 2 && lait != 3) {
                 lait = readLine("Veuillez taper une valeur entre 1 et 2 !\nréessayez.\nCombien de doses ? (max. 3)\n>").toInt
               }
               // fixer le prix du lait et le lait supplémentaire utilisé en fonction du choix de la dose
               prixlait = 0.05 * lait
               laitsupputil = 0.050 * lait
             }
           }
           //interface texte du choix de la boisson et des ingrédients supplémentaire
           println("Boisson sélectionnée : " + nomboisson)
           if (sucre == 1) {
             println("Niveau de sucre : sans sucre")
           }
           else if (sucre != 1) {
             println("Niveau de sucre : Peu (" + ((sucre * 5) - 5) + "g)")
           }
           if (boisson == 2 || boisson == 3) {
             if (laitoui == 2) {
               println("Lait en supplément : Non")
             }
             else if (laitoui == 1) {
               println("Lait en supplément : " + (lait * 50) + "ml")
             }
           }
           //vérifier les stocks de chaque ingrédient si stock suffisant : varianle true et misae à jour des stocks / sinon : message erreur variable reste false
           if (stockcafe >= cafeutil && stocksucre >= sucreutil && stocklait >= (laitutil + laitsupputil)) {
             dispo = true
             stockcafe -= cafeutil
             stocksucre -= sucreutil
             stocklait -= (laitutil + laitsupputil)
           } else if (stockcafe < cafeutil || stocksucre < sucreutil || stocklait < (laitutil + laitsupputil)) {
             if (stockcafe < cafeutil) {
               println("Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionné")
             }
             if (stocksucre < sucreutil) {
               println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionné")
             }
             if (stocklait < laitutil) {
               println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionné")
             }
             println("veuillez choisir une autre boisson ou vérifier les stocks en mode Admin")
             }
           //Interface de paiement: si la variable dispo est true : code twint généré aléatoirement
         }while(!dispo)
         if (dispo) {
           val alphanum: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
           var twint = ""
           for (i <- 1 to 5) {
             val lettre = (Math.random() * 36).toInt
             twint += alphanum(lettre)
           }
           prixtotal = prixcafe + prixsucre + prixlait
           //pour afficher les prix au dixième : 0.10 chf 1.35 chf etcc
           val prixtotal1 = f"$prixtotal%.2f"
           val prixcafe1 = f"$prixcafe%.2f"
           val prixlait1 = f"$prixlait%.2f"
           val prixsucre1 = f"$prixsucre%.2f"
           //en fonction de si on rajoute du sucre et/ou si on rajoute du lait supplémenatire l'interface texte du paiement change.
           if (sucre == 1 && laitoui != 1) {
             println("Prix total : CHF " + prixcafe1 + "\n")}
           else if (sucre == 1 && laitoui == 1) {
             println("Prix total : CHF " + prixcafe1 + " + CHF " + prixlait1 + " = CHF " + prixtotal1 + "\n")}
           else if (sucre != 1 && laitoui != 1) {
             println("Prix total : CHF " + prixcafe1 + " + CHF " + prixsucre1 + " = CHF " + prixtotal1 + "\n")}
           else if (sucre != 1 && laitoui == 1) {
             println("Prix total : CHF " + prixcafe1 + " + CHF " + prixsucre1 + " + CHF " + prixlait1 + " = CHF " + prixtotal1 + "\n")}
// texte de paiement avec le temps d'attente
           println("\nVeuillez payer Via TWINT :\nvotre code de paiement est : " + twint + "\n(En attente de validation du paiement...)")
           Thread.sleep(3000)
           println("\n\nMerci! Votre paiement a été accpeté.\nPréparation de votre boisson...")
           Thread.sleep(5000)
           println("Votre " + nomboisson + " est prêt! Bonne dégustation!\n\n")
         }
       }
//Mode admin
      if (mode == 2) {
          var pin1 = readLine("Bienvenue dans le mode administrateur. Veuillez entrer le code pin à six chiffre : \n>")
          while (pin != pin1) {
            pin1 = readLine("Le code PIN est invalide !\nVeuillez réessayez.\nEntrer le code PIN à six chiffre\n>")}
          println("Voici les stocks de la machine :\n- Poudre de café : " + stockcafe + "g \n- Sucre : " + stocksucre + "g \n- Lait : " + stocklait + "ml\n")
            pluscafe = readLine("Réaprovvisionnement des stocks...\nAjout : Poudre à café : ").toDouble
            plussucre = readLine("Ajout : Sucre : ").toDouble
            pluslait = readLine("Ajout : lait : ").toDouble
            stockcafe += pluscafe
            stocksucre += plussucre
            stocklait += pluslait
          println("Voici les stocks de la machine mis à jour:\n- Poudre de café : " + stockcafe + "g \n- Sucre : " + stocksucre + "g \n- Lait : " + stocklait + "ml\n\nRetour au menu principal...")
        }
    }while (mode == 1 || mode == 2)
    println("Vous quittez le programme. Aurevoir :)")
    }
  }