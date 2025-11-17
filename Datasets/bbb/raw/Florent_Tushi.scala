import scala.io.StdIn._
object Main {
  def main(args: Array[String]): Unit = {

    val nbMachine = 5
    var machinePins = Array.fill(nbMachine)("434343")
    var stocklait = Array.fill(nbMachine)(500)
    var stocksucre = Array.fill(nbMachine)(30)
    var stockcafe = Array.fill(nbMachine)(50)
    var mode = 0
    var admin = 0
    var machineId = 0

    do{//boucle tant que les mode 1 et 2 tournent
    mode = readLine("Nospresso Café\nSélectionnez votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toInt
    while ((mode < 1) || (mode > 3)) {mode = readLine("Erreur... Entrez un chiffre entre 1 et 3\nNospresso Café\nSélectionnez votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toInt}
    if (mode == 1 || mode == 2) {// ce if sert à demander à l'utilisateur la machine quil veut utiliser. Ce serait pas logique de demander lorsqu'il choisi de quitter
      machineId = readLine("Machine sélectionnée (1-5) >").toInt
      while (machineId < 1 || machineId > 5) {machineId = readLine("Erreur... Entrez un chiffre entre 1 et 5\n Machine sélectionnée (1-5) >").toInt}
        machineId-=1 // moins 1 pour que la machine selectionnée match correctement avec l'index dans les tableaux(0-4)
        if (mode == 1) {serveClient(machineId, stockcafe, stocksucre, stocklait)}
        if (mode == 2) {
          if (validatePin(machineId, machinePins)) {//commme validate pin donne une réponse en booléen on peut le mettre dans la condition du if.
            //demander quelle action faire en admin changer le PIN ou réapprovisionner les stocks :
            admin = readLine("1) Changer le mot de passe\n2) Réapprovvisionnement des stocks\n>").toInt
            while ((admin < 1) || (admin > 2)) {admin = readLine("Erreur... Entrez une valeur entre 1 et 2\n1) Changer le mot de passe\n2) Réapprovvisionnement des stocks\n>").toInt}
            if (admin == 1) {updatePin(machineId, machinePins)}
            if (admin == 2) {restockMachine(machineId, stockcafe, stocksucre, stocklait)}
          } else { mode = 3 }//si le code PIN est faux plus de trois fois le programme se fini. Je met mode = 3 afin de quitter la boucle do{}while(mode==1||mode==2)
        }
    }
    }while(mode==1||mode==2)
    println("Vous quittez le programme.")
  }
    // méthode pour valider le code pin de la machine choisie
    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      for (essai <- 1 to 3) {//trois essai pour retourner une réponse True sinon il renvoie un false
        var pin_saisi = readLine("Entrez le code PIN >")
        if (pin_saisi == machinePins(machineId)) {
          println("Accès accordé à la Machine " + (machineId+1))//Pour l'affichage (Index≠Machine sélectionnée)
          return true
        }
        else {
          println("Code PIN incorrect : " + (3 - essai) + " tentatives restantes")
        }
      }//le for s'execute 3 fois seulement et apres on sort de la boucle et retour False
      println("Trop de tentatives échouées. Fin du programme.")
      false
    }

    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      println("Mise à jour du code PIN pour la Machine " + (machineId+1))
      var nouveau_PIN = readLine("Veuillez entrer le nouveau code PIN à 6 chiffres\n>") // apres avoir demander le nouveau code en Int. Apres Boucle while ou je transforme la var nouveau_PIN.toString et je compare la longeur .length pour confirmer une saisie valide de 6 chiffre exactement.
      while ((!nouveau_PIN.forall(_.isDigit)) || (nouveau_PIN.length > 6 || nouveau_PIN.length < 6 ) ) {nouveau_PIN = readLine("Le nouveau code PIN doit comporter exactement 6 chiffres\nVeuillez entrer le nouveau code PIN\n>")}
      machinePins(machineId) = nouveau_PIN.toString
      println("Le code pin à été mis à jour avec succès.\nRetour au menu principal...")
    }
  // quasiment le meme code que dans l'exercice 1 j'ai juste mis les valeurs en Int pour respecter les tableaux.
    def serveClient(machineId: Int, stockcafe: Array[Int], stocksucre: Array[Int], stocklait: Array[Int]): Boolean = {
      //déclarer les variable des prix
      var prixsucre = 0.0
      var prixlait = 0.0
      var prixcafe = 0.0
      var prixtotal = 0.0
      //déclarer les variable de choix de la boisson
      var boisson = 0
      var latte = 0
      var sucre = 0
      var lait = 0
      //déclarer la variable du choix sin on ajouter du lait supplémentaire ou non
      var laitoui = 0
      //déclarer les variables de la quantité nécessaire de chaque ingrédient
      var cafeutil = 0
      var sucreutil = 0
      var laitutil = 0
      //Déclarer la variable qui permet de creer la boucle et d'en sortir
      var dispo = false
      //déclarer une variable en string pour le nom de la boisson en fonction de la boisson choisie
      var nomboisson = "a"
        //Choix de la boisson
        println("\nMode client : Sélection de boisson\n")
        boisson = readLine("Veuillez sélectionner votre boisson\n1) Expresso - CHF 2.00\n2) Capuccino - CHF 2.50\n3) Latte - CHF 2.70 (petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
        while (boisson < 1 ||  boisson > 3) {boisson = readLine("Erreur... Entrez un chiffre entre 1 et 3\nVeuillez sélectionner votre boisson\n1) Expresso - CHF 2.00\n2) Capuccino - CHF 2.50\n3) Latte - CHF 2.70 (petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) >").toInt}
        //fixer le prix et les ingrédients utilisé pour la boisson sélectionner
        if (boisson == 1) {
          nomboisson = "Expresso"
          cafeutil = 8
          prixcafe = 2.00
        }
        else if (boisson == 2) {
          nomboisson = "Cappuccino"
          cafeutil = 6
          laitutil = 100
          prixcafe = 2.50
        }
        else if (boisson == 3) {
          latte = readLine("Veuillez choisir la taille:\n1) Petit\n2) Moyen\n3) Grand\n>").toInt
          while (latte < 1 || latte > 3) {latte = readLine("Erreur... Entrez une valeur entre 1 et 3 !\nVeuillez choisir la taille:\n1) Petit\n2) Moyen\n3) Grand\n>").toInt}
          if (latte == 1) {
            nomboisson = "Latte (Petit)"
            cafeutil = 6
            laitutil = 120
            prixcafe = 2.70
          }
          if (latte == 2) {
            nomboisson = "Latte (Moyen)"
            cafeutil = 8
            laitutil = 150
            prixcafe = 3.20
          }
          if (latte == 3) {
            nomboisson = "Latte (Grand)"
            cafeutil = 12
            laitutil = 200
            prixcafe = 3.70
          }
        }
        //Personnalisation de la boisson
        sucre = readLine("Veuillez choisir la dose de sucre\n1) Pas de sucre\n2) Peu de sucre(5g)\n3) Moyen sucre(10g)\n4) Beaucoup de sucre(15g)\n>").toInt
        while (sucre < 1 || sucre > 4) {
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
        var laitsupputil = 0
        if (boisson == 2 || boisson == 3) {
          //demander si le client du lait supplémentaire seulement si il prend un capuccino ou un latte
          laitoui = readLine("Souhaitez vous ajouter du lait supplémentaire ? \n1) Oui\n2) Non\n>").toInt
          while (laitoui != 1 && laitoui != 2) {
            laitoui = readLine("Veuillez taper une valeur entre 1 et 2 !\nRéessayez.\nSouhaitez vous ajouter du lait supplémentaire ? \n1) Oui\n2) Non\n>").toInt
          }
          if (laitoui == 1) {
            lait = readLine("Combien de doses ? (max. 3)\n>").toInt
            while (lait < 1 || lait > 3) {
              lait = readLine("Veuillez taper une valeur entre 1 et 2 !\nréessayez.\nCombien de doses ? (max. 3)\n>").toInt
            }
            // fixer le prix du lait et le lait supplémentaire utilisé en fonction du choix de la dose
            prixlait = 0.05 * lait
            laitsupputil = 50 * lait
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
        if (stockcafe(machineId) >= cafeutil && stocksucre(machineId) >= sucreutil && stocklait(machineId) >= (laitutil + laitsupputil)) {
          dispo = true
          stockcafe(machineId) -= cafeutil
          stocksucre(machineId) -= sucreutil
          stocklait(machineId) -= (laitutil + laitsupputil)
        } else if (stockcafe(machineId) < cafeutil || stocksucre(machineId) < sucreutil || stocklait(machineId) < (laitutil + laitsupputil)) {
          if (stockcafe(machineId) < cafeutil) {
            println("Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionné")
          }
          if (stocksucre(machineId) < sucreutil) {
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionné")
          }
          if (stocklait(machineId) < laitutil) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionné")
          }
          println("veuillez selectionner une autre machine SVP")        }
        //Interface de paiement: si la variable dispo est true : code twint généré aléatoirement
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
          println("Prix total : CHF " + prixcafe1 + "\n")
        }
        else if (sucre == 1 && laitoui == 1) {
          println("Prix total : CHF " + prixcafe1 + " + CHF " + prixlait1 + " = CHF " + prixtotal1 + "\n")
        }
        else if (sucre != 1 && laitoui != 1) {
          println("Prix total : CHF " + prixcafe1 + " + CHF " + prixsucre1 + " = CHF " + prixtotal1 + "\n")
        }
        else if (sucre != 1 && laitoui == 1) {
          println("Prix total : CHF " + prixcafe1 + " + CHF " + prixsucre1 + " + CHF " + prixlait1 + " = CHF " + prixtotal1 + "\n")
        }
        // texte de paiement avec le temps d'attente
        println("\nVeuillez payer Via TWINT :\nvotre code de paiement est : " + twint + "\n(En attente de validation du paiement...)")
        Thread.sleep(3000)
        println("\n\nMerci! Votre paiement a été accpeté.\nPréparation de votre boisson...")
        Thread.sleep(5000)
        println("Votre " + nomboisson + " est prêt! Bonne dégustation!\n\n")
        return true // car la methode doit renvoyer une réponse booléene
      }
      false // idem deux ligne au dessus
    }
    def restockMachine(machineId: Int, stockcafe: Array[Int], stocksucre: Array[Int], stocklait: Array[Int]): Unit = {
      var pluscafe = 0
      var plussucre = 0
      var pluslait = 0
      println("Voici les stocks de la machine "+(machineId+1)+":\n- Poudre de café : " + stockcafe(machineId) + "g \n- Sucre : " + stocksucre(machineId) + "g \n- Lait : " + stocklait(machineId) + "ml\n\nRéaprovvisionnement des stocks...\n")
      pluscafe = readLine("Ajout : Poudre à café : ").toInt
      while (pluscafe<0) {pluscafe= readLine("Veuillez entrer une valeure positive ! \nAjout : Poudre à café : ").toInt}
      plussucre = readLine("Ajout : Sucre : ").toInt
      while (plussucre<0) {plussucre= readLine("Veuillez entrer une valeure positive ! \nAjout : Sucre : ").toInt}
      pluslait = readLine("Ajout : lait : ").toInt
      while (pluslait<0) {pluslait= readLine("Veuillez entrer une valeure positive ! \nAjout : Lait : ").toInt}
      stockcafe(machineId) += pluscafe
      stocksucre(machineId) += plussucre
      stocklait(machineId) += pluslait
      println("Voici les stocks mis à jour:\n- Poudre de café : " + stockcafe(machineId) + "g \n- Sucre : " + stocksucre(machineId) + "g \n- Lait : " + stocklait(machineId) + "ml\n\nRetour au menu principal...")
    }
  }