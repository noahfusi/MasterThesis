import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import scala.io.Source

object Main {

  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

    def addIngredient(ingredient: String, amount: Int): Unit = {
      //On ajoute les quantités
      if (ingredient == "coffee") coffee += amount
      else if (ingredient == "milk") milk += amount
      else sugar += amount
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      //On teste les quantités, puis retire si assez, pour l'ingrédient choisi
      if (ingredient == "coffee") {
        if (amount > coffee) return false
        else {
          coffee -= amount
          return true
        }
      }

      else if (ingredient == "milk") {
        if (amount > milk) return false
        else {
          milk -= amount
          return true
        }
      }

      else {
        if (amount > sugar) return false
        else {
          sugar -= amount
          return true
        }
      }
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {

    var machines = ArrayBuffer[Machine]()

    //On vérifie les erreurs pouvant arriver lors de la lecture
    try {
      println("Chargement des machines depuis "+filename)
      val fr = Source.fromFile(filename)
      val lignefr = fr.reset.getLines
      var ligne = lignefr.next()

      var nbMachine = 1
      while (!lignefr.isEmpty){
        var ligne = lignefr.next
        var objet = ligne.split(",")
        machines += new Machine(nbMachine,objet(0),objet(1).toInt,objet(2).toInt,objet(3).toInt)
        nbMachine += 1
      }

      fr.close()
      nbMachine -= 1
      println("\n"+nbMachine+" machine(s) chargée(s) avec succès")
    }
    catch {
      case _ => println("Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
    }
    return machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {

    //On vérifie les erreurs pouvant arriver lors de l'écriture
    try {
      val fw = new PrintWriter(filename)
      fw.println("PINCODE,MILK,SUGAR,COFFEE")

      for (machineObj <- machines) {
        fw.println(machineObj.pincode + "," + machineObj.milk + "," + machineObj.sugar + "," + machineObj.coffee)
      }
      var nbMachines = machines.length
      println("Sauvegarde de "+nbMachines+" machines dans "+filename)
      fw.close()
    }
    catch {
      case _ => println("Erreur : Echec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
        println("---\nErreur : echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }
  }


  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    val  pinUtil = readLine("Enter PIN: ")
    if (machines(machineId).pincode == pinUtil) {
      return true
    } else {
      return false
    }
  }

  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    var nouveauPin = 0

    //Test pour vérifier que le code PIN contienne bien 6 chiffres
    while(nouveauPin.toString.length != 6) {
      try {
        nouveauPin = readLine("Entrez un nouveau code PIN a 6 chiffres > ").toInt
      }
      catch{
        case _ => println("Le nouveau code PIN doit contenir uniquement 6 chiffres")
      }
    }
    machines(machineId).pincode = nouveauPin.toString
  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean ={
    // On a initialiser cette variable booleen a faux pour pouvoir rentrer une premiere fois dans la boucle while
    var sansErreur = true
    // On creer des variables pour avoir des compteurs de quantite d'ingredients
    var cafeNecess = 0
    var sucreNecess = 0
    var laitNecess = 0

    // Afficher la page de choix de boissons
    var boisson = 0
    while ((boisson < 1) || (boisson > 3)) {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20(Moyen), CHF 3.70 (Grand)")
      boisson = readLine(">").toInt
    }

    // Deux variable pour stocker le nom et le prix de boisson
    var boissonNom = ""
    var boissonPrix = 0.00

    // Sachant le choix de boisson de l'utilisateur on affecte au variable les donnees necessaire
    if (boisson == 1) {
      boissonNom = "Expresso"
      boissonPrix = 2.00
      cafeNecess = 8
    }
    else if (boisson == 2) {
      boissonNom = "Cappuccino"
      boissonPrix = 2.50
      cafeNecess = 6
      laitNecess = 100
    }
    else if (boisson == 3) {
      // Puisque le latte est le seule a avoir des tailles on lui creer une page de selection de taille
      // Ainsi on lui creer une variable pour memoriser la taille
      var latteTaille = 0
      while ((latteTaille < 1) || (latteTaille > 3)) {
        println("Sélectionnez la taille de votre latte:")
        println("1) CHF 2.70 (Petit)")
        println("2) CHF 3.20 (Moyen)")
        println("3) CHF 3.70 (Grand)")
        latteTaille = readLine(">").toInt
      }

      // Sachant la taille du Latte entree par l'utilisateur, on affecte au variable tout les donnees necessaire
      if (latteTaille == 1) {
        boissonNom = "Latte (Small)"
        boissonPrix = 2.70
        cafeNecess = 6
        laitNecess = 120
      }
      else if (latteTaille == 2) {
        boissonNom = "Latte (Medium)"
        boissonPrix = 3.20
        cafeNecess = 8
        laitNecess = 150
      }
      else if (latteTaille == 3) {
        boissonNom = "Latte (Large)"
        boissonPrix = 3.70
        cafeNecess = 12
        laitNecess = 200
      }
    }
    // Page pour le choix de niveaux sucre
    var niveauSucre = 0
    while ((niveauSucre < 1) || (niveauSucre > 4)) {
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      niveauSucre = readLine(">").toInt
    }
    // pareil qu'avant grace au deux nouveaux variable on pourras stocker les donnes voulu concernant le niveaux de sucre
    var sucreNom = ""
    var sucrePrix = 0.00
    //Sachant le niveaux de sucre choisi, on affecte les donnes necessaire
    if (niveauSucre == 1) {
      sucreNom = "Sans sucre"
      sucrePrix = 0
      sucreNecess = 0
    }
    else if (niveauSucre == 2) {
      sucreNom = "Peu (5g)"
      sucrePrix = 0.10
      sucreNecess = 5
    }
    else if (niveauSucre == 3) {
      sucreNom = "Moyen (10g)"
      sucrePrix = 0.20
      sucreNecess = 10
    }
    else if (niveauSucre == 4) {
      sucreNom = "Beaucoup (15g)"
      sucrePrix = 0.30
      sucreNecess = 15
    }

    var laitSupp = 0
    var dose = 0

    // Grace aux deux nouveaux variables on pourra stocker les donnees voulu concernant l'ajout de lait sup
    var laitNom = ""
    var laitPrix = 0.0
    // Cette condition verifie que l'ajout supp de lait est que possible pour le cappuccino et le latte
    if ((boisson == 2) || (boisson == 3)) {

      while ((laitSupp < 1) || (laitSupp > 2)) {
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("(Disponible uniquement pour Cappuccino et Latte)")
        println("1) Oui")
        println("2) Non")
        laitSupp = readLine(">").toInt
      }
      // Si on choisit d'ajouter deu lait
      if (laitSupp == 1) {
        laitNom = "Oui"
        // cette condition s'assure que l'utilisateur ajoute au moins 1 dose et 3 doses maximale par boisson sinon boucle
        // (J'utilise la negation de ce propos pour pouvoir ecrire une condition moins longue)
        while ((dose <= 0) || (dose > 3)) {
          println("Combien de dose ?")
          dose = readLine(">").toInt
        }
        laitPrix = dose * 0.05
        laitNecess = laitNecess + (dose * 50)
      }
      else if (laitSupp == 2) {
        laitNom = "Non"
      }
    }

    // Gestions des erreurs possibles pour les boissons 1 et 2
    //(On verifie les memes trucs meme si l'expresso utilise que de la poudre de cafe)
    if ((boisson == 1) || (boisson == 2)) {
      if (laitNecess > machines(machineId).milk) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        println("Veuillez choisir une autre machine.")
        sansErreur = false
      }
      if (cafeNecess > machines(machineId).coffee) {
        println(" Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        println("Veuillez choisir une autre machine.")
        sansErreur = false
      }
      if (sucreNecess > machines(machineId).sugar) {
        println(" Erreur : Quantité de sucre de café insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        println("Veuillez choisir une autre machine.")
        sansErreur = false
      }
    }
    //Gestion d'erreur pour la boisson 3
    else if (boisson == 3) {
      if (laitNecess > machines(machineId).milk) {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        println("Veuillez choisir une autre machine.")
        sansErreur = false
      }
      else if (cafeNecess > machines(machineId).coffee) {
        println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        println("Veuillez choisir une autre machine.")
        sansErreur = false
      }
      else if (sucreNecess > machines(machineId).sugar) {
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        println("Veuillez choisir une autre machine.")
        sansErreur = false
      }
    }

    // Condition permettant de continuer a payer si il n'ya pas d'erreur
    if (sansErreur) {
      val final_price = boissonPrix + sucrePrix + laitPrix
      if (laitSupp == 1) {
        println("Boisson sélectionnée : " + boissonNom)
        println("Niveau de sucre : " + sucreNom)
        println("Lait en supplément : " + laitNom)
        println("Dose : " + dose)
        println("Prix total : CHF " + boissonPrix + " + CHF " + sucrePrix + " + CHF " + laitPrix + " = CHF " + final_price)
      }
      else if (laitSupp == 2) {
        println("Boisson sélectionnée : " + boissonNom)
        println("Niveau de sucre : " + sucreNom)
        println("Lait en supplément : " + laitNom)
        println("Prix total : CHF " + boissonPrix + " + CHF " + sucrePrix + " = CHF " + final_price)
      }

      println("Veuillez payer en utilisant Twint.")
      // Un string qui est aleatoirement generer et est composer de 5 caracteres alphanumerique
      val CharPossible = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      var codeTwint = ""
      for (i <- 0 to 4) {
        codeTwint = codeTwint + CharPossible((math.random() * CharPossible.length).toInt)
      }
      println("Votre code de paiement est : " + codeTwint)
      println("(En attente de validation du paiement...)")
      Thread.sleep(3000) // Attend pendant 3000 millisecondes (31 secondes)
      println("\nMerci ! Votre paiement a été accepté.")

      println("Préparation de votre boisson...")
      println("[...]")
      println("Votre " + boissonNom + " est prêt ! Bonne dégustation !")

      machines(machineId).removeIngredient("coffee",cafeNecess)
      machines(machineId).removeIngredient("milk",laitNecess)
      machines(machineId).removeIngredient("sugar",sucreNecess)
    }

    //reinitialise toute les valeur de la boisson personaliser pour pouvoir retourner et refaire une boisson

    boisson = 0
    niveauSucre = 0
    laitSupp = 0
    dose = 0

    if (!sansErreur) {
      return false
    }
    else{
      return true
    }

  }

  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    //montre les stocks maintenant
    println("\nStocks:")
    println("  Poudre de café: " + machines(machineId).coffee + "g")
    println("  Lait          : " + machines(machineId).milk + "mL")
    println("  Sucre         : " + machines(machineId).sugar + "g")

    println("\nRéapprovisionnement des stocks...")
    //permet d'ajouter des quantites specifique de chaque
    println("Ajout:")
    val coffee_add = readLine("  Poudre de café: ").toInt
    machines(machineId).addIngredient("coffee",coffee_add)

    val milk_add = readLine("  Lait          : ").toInt
    machines(machineId).addIngredient("milk",milk_add)

    val sugar_add = readLine("  Sucre         : ").toInt
    machines(machineId).addIngredient("sugar",sugar_add)


    println("Niveaux de stock mis à jour.")
    println("Retour au menu principal...")
  }


  def main(args: Array[String]): Unit = {


    //Variable qui contient le nom du fichier
    var filename = "machines.csv"

    //On initialise l'arrayBuffer avec les machines
    var machines = loadcsv(filename)

    //On met cette variable a true pour pouvoir entrer dans la boucle while une premiere fois
    // Sauf si le chargement du fichier n'a pas marché
    var reaffiche = true
    if (machines.isEmpty){
      reaffiche = false
      println("---\nErreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
    }


    // On a initialiser une variable reafficher pour determiner si on reaffiche  le menu de selection des modes
    while (reaffiche) {
      reaffiche = false
      // On affiche la page de sélection de mode
      var mode = 0
      while ((mode < 1) || (mode > 3)) {
        println("Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        mode = readLine(">").toInt
      }

      //Cette condition s'execute si l'utilisateur a bien choisit 1
      if (mode == 1) {
        var machineId = readLine("Machine sélectionnée (1-5) > ").toInt
        while ((machineId > 5) || (machineId < 1)) {
          machineId = readLine("Machine sélectionnée (1-5) > ").toInt
        }
        machineId -=1
        val serve = serveClient(machineId, machines)

        reaffiche = true
        mode = 0
      }
      //Mode admin
      else if (mode == 2) {

        // Demande au admin d'ecrire le pin si faut il redemande
        println("Mode Admin")
        var machineId = readLine("Machine sélectionnée (1-5) > ").toInt
        while ((machineId > 5) || (machineId < 1)) {
          machineId = readLine("Machine sélectionnée (1-5) > ").toInt
        }
        machineId -=1

        var validite = validatePin(machineId, machines)
        var tentative = 3
        while ((tentative != 0 ) && (validite == false)) {
          tentative -= 1
          validite = validatePin(machineId, machines)
        }

        // apres avoir saisie le bon code il continue
        println("Accès autorisé.")

        var i = 0
        while ((i < 1) || (i > 3)) {
          println("Veuillez sélectionner votre mode :")
          println("1) Réapprovisioner les ingredients")
          println("2) Update code pin")
          i = readLine(">").toInt
        }
        if(i == 1) {
          restockMachine(machineId, machines)
          //reinitialise les valeurs necessaire pour retourner a la page de slection de mode
          reaffiche = true
          mode = 0
          i = 0
        } else {
          updatePin(machineId, machines)
          println("Le code pin a été mis a jour avec succès")
          reaffiche = true
          mode = 0
          i = 0
        }
      }
      else if (mode==3) savecsv(filename,machines)
    }
  }
}