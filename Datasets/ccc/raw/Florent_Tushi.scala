import java.io.{FileNotFoundException, IOException, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn._
//déclaration de la class Machine avec les nouvelles variables stocké dans le tableau
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {//à mettre en place dans le restockmachine pour la gestion des stocks
    if (ingredient == "milk") {
      milk += amount
    } else if (ingredient == "sugar") {
      sugar += amount
    } else if (ingredient == "coffee") {
      coffee += amount
    }
  }
  def removeIngredient(ingredient: String, amount: Int): Boolean = {//à mettre en place dans le serveclient pour la gestion des stocks
    if (ingredient == "milk") {
        milk -= amount
    }else if (ingredient == "sugar") {
        sugar -= amount
    } else if (ingredient == "coffee") {
        coffee -= amount
    }
    true
  }
}
object Main {
  def loadcsv(filename: String): ArrayBuffer[Machine] = {//lire le fichier machines.csv et configurer le tableau machines
    val machines = ArrayBuffer[Machine]()
    try {
      val fichier = Source.fromFile(filename)
      val ligne = fichier.getLines().drop(1)
      var numid = 1
      for (l <- ligne) {
        val donnee = l.split(",")
        if (donnee.length == 4) {
          val id = numid
          val pincode = donnee(0)
          val milk = donnee(1).toInt
          val sugar = donnee(2).toInt
          val coffee = donnee(3).toInt
          machines += new Machine(id, pincode, milk, sugar, coffee) //créer les tableaux de chaque machines dans le arraybuffer Machine
          numid = numid + 1
        }
      }
    } catch {
      case _: FileNotFoundException =>// en cas de fichier manquant
        println("Erreur : Fichier introuvable. Vérifiez le chemin d’acces et réessayez.")
        println("---")
        println("Erreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.: ")

        sys.exit(1)
    }
    machines //la methode renvoie un arraybuffer qu'on a déclarer juste avant dcp
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {//réécris le machines.csv après toute les opérations
    try{
      val fw = new PrintWriter(filename)
      fw.println("PINCODE,MILK,SUGAR,COFFEE")
      for(y<-machines){
        val nvligne = y.pincode+","+y.milk+","+y.sugar+","+y.coffee
        fw.println(nvligne)
      }
      fw.close()
      println("Sauvegarde de "+machines.length+" machines dans machines.csv...\nFichier sauvegardé avec succès.")
    }catch {
      case _: IOException =>
        println("Erreur : Echec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    val machines = loadcsv(filename)
    for (x <- machines) {//afficher les stocks de chaque machines !
      println("Machine " + x.id + " chargée :\n\tID: " + x.id + "\n\tCode Pin: " + x.pincode + "\n\tLait: " + x.milk + " ml\n\tSucre: " + x.sugar + " g\n\tPoudre à café: " + x.coffee+" g\n")
    }
    var mode = 0
    var admin = 0
    do { //boucle tant que les mode 1 et 2 tournent
      mode = readLine("Nospresso Café\nSélectionnez votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toInt
      while ((mode < 1) || (mode > 3)) {
        mode = readLine("Erreur... Entrez un chiffre entre 1 et 3\nNospresso Café\nSélectionnez votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toInt
      }
      if (mode == 1 || mode == 2) {
        val machineId =choixmachine(machines)// Enfaite c'était pas forcément nécessaire de faire une méthode choix machines mais tant pis ça marche tout aussi bien // ce if sert à demander à l'utilisateur la machine quil veut utiliser. Ce serait pas logique de demander lorsqu'il choisi de quitter
        if (mode == 1) {
          serveClient(machines,machineId)
        }
        if (mode == 2) {
          if (validatePin(machines,machineId)) { //comme validate pin donne une réponse en booléen on peut le mettre dans la condition du if.
            //demander quelle action faire en admin changer le PIN ou réapprovisionner les stocks :
            admin = readLine("1) Changer le mot de passe\n2) Réapprovvisionnement des stocks\n>").toInt
            while ((admin < 1) || (admin > 2)) {
              admin = readLine("Erreur... Entrez une valeur entre 1 et 2\n1) Changer le mot de passe\n2) Réapprovvisionnement des stocks\n>").toInt
            }
            if (admin == 1) {
              updatePin(machines,machineId)
            }
            if (admin == 2) {
              restockMachine(machines,machineId)
            }
          }
          else {mode = 3 //si le code PIN est faux plus de trois fois le programme se fini. Je met mode = 3 afin de quitter la boucle do{}while(mode==1||mode==2)
          }
        }
      }
    }while (mode == 1 || mode == 2)
    if (mode == 3){
    savecsv(filename,machines)

    println("Vous quittez le programme.")}
  }
  def choixmachine(machines:ArrayBuffer[Machine]): Int ={
    val nbmachine = machines.length
    var choix = readLine("Machine sélectionnée (1-"+nbmachine+") >").toInt
    while (choix < 1 || choix > nbmachine) {
      choix = readLine("Erreur... Entrez un chiffre entre 1 et 5\n Machine sélectionnée (1-"+nbmachine+") >").toInt
    }
    choix - 1 // moins 1 pour que la machine selectionnée match correctement avec l'index dans les tableaux(0-4)
  }

  // méthode pour valider le code pin de la machine choisie
  def validatePin(machines: ArrayBuffer[Machine],machineId:Int): Boolean = {
    for (essai <- 1 to 3) { //trois essai pour retourner une réponse True sinon il renvoie un false
      val pin_saisi = readLine("Entrez le code PIN >")
      if (pin_saisi == machines(machineId).pincode) {
        println("Accès accordé à la Machine " + (machineId + 1)) //Pour l'affichage (Index≠Machine sélectionnée)
        return true
      }
      else {
        println("Code PIN incorrect : " + (3 - essai) + " tentatives restantes")
      }
    } //le for s'execute 3 fois seulement et apres on sort de la boucle et retour False
    println("Trop de tentatives échouées. Fin du programme.")
    false
  }


  def updatePin(machines: ArrayBuffer[Machine],machineId:Int): Unit = {
    println("Mise à jour du code PIN pour la Machine " + (machineId + 1))
    var nouveau_PIN = readLine("Veuillez entrer le nouveau code PIN à 6 chiffres\n>") // apres avoir demander le nouveau code en Int. Apres Boucle while ou je transforme la var nouveau_PIN.toString et je compare la longeur .length pour confirmer une saisie valide de 6 chiffre exactement.
    while ((!nouveau_PIN.forall(_.isDigit)) || (nouveau_PIN.length > 6 || nouveau_PIN.length < 6)) {
      nouveau_PIN = readLine("Le nouveau code PIN doit comporter exactement 6 chiffres\nVeuillez entrer le nouveau code PIN\n>")
    }
    machines(machineId).pincode = nouveau_PIN
    println("Le code pin à été mis à jour avec succès.\nRetour au menu principal...")
  }

  // quasiment le meme code que dans l'exercice 1 j'ai juste mis les valeurs en Int pour respecter les tableaux.
  def serveClient(machines : ArrayBuffer[Machine],machineId:Int): Boolean = {
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
    //déclarer la variable du choix si on ajouter du lait supplémentaire ou non
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
    while (boisson < 1 || boisson > 3) {
      boisson = readLine("Erreur... Entrez un chiffre entre 1 et 3\nVeuillez sélectionner votre boisson\n1) Expresso - CHF 2.00\n2) Capuccino - CHF 2.50\n3) Latte - CHF 2.70 (petit), CHF 3.20 (Moyen), CHF 3.70 (Grand) >").toInt
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
      laitutil = 100
      prixcafe = 2.50
    }
    else if (boisson == 3) {
      latte = readLine("Veuillez choisir la taille:\n1) Petit\n2) Moyen\n3) Grand\n>").toInt
      while (latte < 1 || latte > 3) {
        latte = readLine("Erreur... Entrez une valeur entre 1 et 3 !\nVeuillez choisir la taille:\n1) Petit\n2) Moyen\n3) Grand\n>").toInt
      }
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
    if (machines(machineId).coffee >= cafeutil && machines(machineId).sugar >= sucreutil && machines(machineId).milk >= (laitutil + laitsupputil)) {
      dispo = true
      machines(machineId).removeIngredient("coffee",cafeutil)//methode removeingredient pour mettre à jour les stocks
      machines(machineId).removeIngredient("sugar", sucreutil)
      machines(machineId).removeIngredient("milk", laitutil + laitsupputil)
    } else if (machines(machineId).coffee < cafeutil || machines(machineId).sugar < sucreutil || machines(machineId).milk < (laitutil + laitsupputil)) {
      if (machines(machineId).coffee < cafeutil) {
        println("Erreur : Quantité de poudre à café insuffisante pour préparer la boisson sélectionné")
      }
      if (machines(machineId).sugar < sucreutil) {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionné")
      }
      if (machines(machineId).milk < laitutil) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionné")
      }
      println("veuillez selectionner une autre machine SVP")        }
    //

    //Interface de paiement: si la variable dispo est true : code twint généré aléatoirement
    if (dispo) {
      val alphanum: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var twint = ""
      for (_ <- 1 to 5) {
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

  def restockMachine(machines :ArrayBuffer[Machine],machineId:Int): Unit = {
    var pluscafe = 0
    var plussucre = 0
    var pluslait = 0
    println("Voici les stocks de la machine " + (machineId + 1) + ":\n- Poudre de café : " + machines(machineId).coffee + "g \n- Sucre : " + machines(machineId).sugar + "g \n- Lait : " + machines(machineId).milk + "ml\n\nRéaprovvisionnement des stocks...\n")
    pluscafe = readLine("Ajout : Poudre à café : ").toInt
    while (pluscafe < 0) {
      pluscafe = readLine("Veuillez entrer une valeure positive ! \nAjout : Poudre à café : ").toInt
    }
    plussucre = readLine("Ajout : Sucre : ").toInt
    while (plussucre < 0) {
      plussucre = readLine("Veuillez entrer une valeure positive ! \nAjout : Sucre : ").toInt
    }
    pluslait = readLine("Ajout : lait : ").toInt
    while (pluslait < 0) {
      pluslait = readLine("Veuillez entrer une valeure positive ! \nAjout : Lait : ").toInt
    }
    machines(machineId).addIngredient("coffee",pluscafe)//methode addingredient pour mettre a jours les stocks
    machines(machineId).addIngredient("milk",pluslait)
    machines(machineId).addIngredient("sugar",plussucre)

    println("Voici les stocks mis à jour:\n- Poudre de café : " + machines(machineId).coffee + "g \n- Sucre : " + machines(machineId).sugar + "g \n- Lait : " + machines(machineId).milk + "ml\n\nRetour au menu principal...")
  }
}