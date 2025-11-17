import io.StdIn._
import scala.collection.mutable.ArrayBuffer
import util.Random
import scala.io.Source
import java.io._

object Main {
  class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int,  var coffee: Int)
  {
    //Ajoute une quantité spécifiée d’un ingrédient au stock de la machine
    def addIngredient(ingredient: String, amount: Int): Unit = {
      ingredient.toLowerCase match {
        case "milk" => milk += amount
        case "sugar" => sugar += amount
        case "coffee" => coffee += amount
        case _ => println("\nIngrédient inconnu")
      }
    }

    //Retire une quantité spécifiée d’un ingrédient du stock de la machine si le stock est suffisant
    def removeIngredient(ingredient: String, amount: Int): Unit = {
      ingredient.toLowerCase match {
        case "milk" if milk >= amount => milk -= amount;
        case "sugar" if sugar >= amount => sugar -= amount;
        case "coffee" if coffee >= amount => coffee -= amount;
        case _ => println("\nStock insuffisant ou ingredient inconnu");
      }
    }
  }

  //Lit le fichier CSV et retourne une collection ArrayBuffer[Machine]
  def loadcsv(filename: String): ArrayBuffer[Machine] ={
    println(f"Chargement des machines depuis $filename...")
    val machines = ArrayBuffer[Machine]()
    val source = Source.fromFile(filename)
    try {
      val lines = source.getLines().drop(1)
      for ((line, index) <- lines.zipWithIndex) {
        val Array(pin, milk, sugar, coffee) = line.split(",")
        val machine = new Machine(index + 1, pin, milk.toInt, sugar.toInt, coffee.toInt)
        machines += machine
        println(f"Machine ${machine.id} chargée :")
        println(f"\tID:\t${machine.id}")
        println(f"\tCode PIN:\t${machine.pincode}")
        println(f"\tLait:\t\t\t:\t${machine.milk / 1000.0}%.2fL")
        println(f"\tSucre:\t\t\t:\t${machine.sugar}g")
        println(f"\tCafé:\t\t\t:\t${machine.coffee}g")
      }
      println(f"\n${machines.length} machine(s) chargée(s) avec succès.")
    } catch{
      case _: FileNotFoundException =>
        println("\nFichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        sys.exit(1)
      case ex: Exception =>
        println("Échec du chargement ou de la sauvegarde des machines.")
        sys.exit(1)
    } finally {
      source.close()
    }
    machines
  }

  //Sauvegarde l’état actuel de toutes les machines dans le fichier CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try{
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach{ m =>
        writer.println(s"${m.pincode},${m.milk},${m.sugar},${m.coffee}")
      }
      writer.close()
      println("\nFichier sauvegardé")
    } catch {
      case _: FileNotFoundException =>
        println("\nFichier introuvable. Vérifiez le chemin d’accès et réessayez.")
      case _: IOException =>
        println("Échec de l’écriture dans machines.csv.")
        println("Le fichier peut être verrouillé ou en lecture seule.")
      case ex: Exception =>
        println("Échec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
    }
  }

  def main(args: Array[String]): Unit = {
    //Décalration des variables
    val machines = loadcsv("machines.csv")
    var enMarche = true

    //Début du programme
    while (enMarche){
      //Choix du mode
      println("\n\tNospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      readLine("> ").toIntOption.getOrElse(0) match {
        case 1 =>
          val currentMachine = readLine(f"\nMachine sélectionnée (1-${machines.length}) > ").toIntOption.getOrElse(0) - 1
          if (currentMachine >= 0 && currentMachine < machines.length){
            serveClient(machines(currentMachine))
          }else{
            println("\nEntrée invalide")
          }
        case 2 =>
          val currentMachine = readLine(f"\nMachine sélectionnée (1-${machines.length}) > ").toIntOption.getOrElse(0) - 1
          if (currentMachine >= 0 && currentMachine < machines.length && validatePin(machines(currentMachine))){
            println("\n1) Réapprovisionner les stocks")
            println("2) Mettre à jour le code PIN")
            readLine("> ").toIntOption.getOrElse(0) match {
              case 1 => restockMachine(machines(currentMachine))
              case 2 => updatePin(machines(currentMachine))
              case _ => println("\nEntrée invalide")
            }
          }else{
            println("\nTrop de tentatives échouées.")
          }
        case 3 =>
          println(f"\nSauvegarde de ${machines.length} machines dans machines.csv...")
          savecsv("machines.csv", machines)
          println("Fichier sauvegardé avec succès.")
          enMarche = false
        case _ => println("\nEntrée invalide")
      }
    }
  }

  //Valider le code PIN pour la machine sélectionnée
  def validatePin(machine: Machine): Boolean = {
    var tentative = 3

    while (tentative > 0){
      println("Entrez le code PIN :")
      var Pin = readLine("> ")
      if (Pin == machine.pincode){
        println(f"Accès accordé à la Machine ${machine.id}")
        return true
      }else{
        tentative -= 1
        println(f"Code PIN incorrect. $tentative tentatives restantes.")
      }
    }
    return false
  }

  //Mettre à jour le code PIN de la machine sélectionnée
  def updatePin(machine: Machine): Unit = {
    var nouveauPin = ""
    println(f"Mise à jour du code PIN pour la machine ${machine.id}.")

    do{
      nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }while(nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit))
    machine.pincode = nouveauPin

    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  //Traiter une transaction client pour la machine sélectionnée
  def serveClient(machine: Machine): Boolean ={
    val prixExpresso = 2.00
    val prixCappuccino = 2.50
    val prixLatteS = 2.70
    val prixLatteM = 3.20
    val prixLatteL = 3.70
    val prixSucre = 0.10

    val erreur = "\nEntrée invalide"

    var boisson = 0
    var nomBoisson = ""
    var tailleLatte = 0
    var nomTailleLatte = ""
    var nivSucre = 0
    var nomNivSucre = ""
    var nivLait = 0
    var nomNivLait = ""
    var cafeRequis = 0
    var sucreRequis = 0
    var laitRequis = 0
    var prix = 0.00

    //Choix de la boisson
    println("\nVeuillez sélectionner votre boisson :")
    println(f"1) Expresso - CHF $prixExpresso%.2f")
    println(f"2) Cappuccino - CHF $prixCappuccino%.2f")
    println(f"3) Latte - CHF $prixLatteS%.2f (Petit), CHF $prixLatteM%.2f (Moyen), CHF $prixLatteL%.2f (Grand)")

    boisson = readLine("> ").toInt

    if (boisson == 1) {
      nomBoisson = "Expresso"
      prix += prixExpresso
      cafeRequis = 8
    }else if (boisson == 2) {
      nomBoisson = "Cappuccino"
      prix += 2.50
      cafeRequis = 6
      laitRequis = 100
    }else if (boisson == 3) {
      println("\nVeuillez choisir la taille du Latte : ")
      println("1) Petit")
      println("2) Moyen")
      println("3) Grand")

      tailleLatte = readLine("> ").toInt
      nomBoisson = "Latte"
      if (tailleLatte == 1) {
        prix += 2.70
        cafeRequis = 6
        laitRequis = 120
        nomTailleLatte = "(Petit)"
      }else if (tailleLatte == 2) {
        prix += 3.20
        cafeRequis = 8
        laitRequis = 150
        nomTailleLatte = "(Moyen)"
      }else if (tailleLatte == 3) {
        prix += 3.70
        cafeRequis = 12
        laitRequis = 200
        nomTailleLatte = "(Grand)"
      }else {
        println(erreur)
        return false
      }
    }else{
      println(erreur)
      return false
    }

    //Choix taux de sucre
    println("\nSouhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println(f"2) Peu (5g) - CHF $prixSucre%.2f")
    println(f"3) Moyen (10g) - CHF ${prixSucre * 2}%.2f")
    println(f"4) Beaucoup (15g) - CHF ${prixSucre * 3}%.2f")

    nivSucre = readLine("> ").toInt

    if (nivSucre == 1){
      nomNivSucre = "Sans sucre"
    }else if (nivSucre == 2){
      nomNivSucre = "Peu (5g)"
      sucreRequis = 5
      prix += prixSucre
    }else if (nivSucre == 3){
      nomNivSucre = "Moyen (10g)"
      sucreRequis = 10
      prix += prixSucre*2
    }else if (nivSucre == 4){
      nomNivSucre = "Beaucoup (15g)"
      sucreRequis = 15
      prix += prixSucre*3
    }else{
      println(erreur)
      return false
    }

    //Choix taux de lait pour Cappuccino et Latte
    if (boisson == 2 || boisson == 3) {
      println("\nSouhaitez-vous ajouter du lait en suppléement ?")
      println("(Disponible uniquement pour Cappuccino et Latte)")
      println("1) Oui")
      println("2) Non")

      var LaitSuppl = readLine("> ").toInt

      //Choix nombre de dose
      if (LaitSuppl == 1) {
        nomNivLait = "Oui"
        println("\nCombien de dose (1-3) ?")
        nivLait += readLine("> ").toInt
        laitRequis += nivLait * 50
        prix += nivLait * 0.05
      }else if (LaitSuppl == 2){
        nomNivLait = "Non"
      }else{
        println(erreur)
        return false
      }
    }

    //Vérification des stocks
    if (machine.coffee >= cafeRequis && machine.sugar >= sucreRequis && machine.milk >= laitRequis){
      println(f"\nBoisson sélectionnée : $nomBoisson")
      println(f"Niveau de sucre : $nomNivSucre")
      println(f"Lait supplémentaire: $nomNivLait")
      println(f"Prix total : $prix%.2f")

      //Paiement
      val twint = Random.alphanumeric.take(5).mkString("")
      println("\nVeuillez payer en utilisant Twint.")
      println(f"Votre code de paiement est : $twint")
      println(f"(En attente de validation du paiement...)")

      //Pause de 3 secondes
      Thread.sleep(3000)
      println("\nMerci ! Votre paiement a été accepté.")

      //Mise à jour des stocks
      machine.removeIngredient("sugar", sucreRequis)
      machine.removeIngredient("coffee", cafeRequis)
      machine.removeIngredient("milk", laitRequis)

      println("\nPréparation de votre boisson...")
      println("[...]")

      //Pause de 5 secondes
      Thread.sleep(5000)
      println(f"Votre $nomBoisson est prêt ! Bonne dégustation !")
      return true
    }else {
      if (machine.coffee < cafeRequis){
        println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
      }else if (machine.sugar < sucreRequis){
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      }else if (machine.milk < laitRequis){
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      }
      println("Veuillez choisir une taille plus petite, essayez une autre boisson ou choisissez une autre machine.")
      return false
    }
  }

  //Réapprovisionner les ingrédients pour la machine sélectionnée
  def restockMachine(machine: Machine): Unit = {
    //Affichage des stocks
    println("\nNiveaux de stock actuels :")
    println("\tPoudre de café\t:\t" + machine.coffee + " g")
    println("\tSucre\t\t\t:\t" + machine.sugar + " g")
    println(f"\tLait\t\t\t:\t${machine.milk/1000.0}%.2f L")

    //Réapprovisionnement
    println("\nRéapprovisionnement des stocks ...:")
    println("Ajout :")
    print("\tPoudre de café\t:\t")
    machine.addIngredient("coffee", readInt())
    print("\tSucre\t\t\t:\t")
    machine.addIngredient("sugar", readInt())
    print("\tLait\t\t\t:\t")
    machine.addIngredient("milk", (readFloat()*1000).toInt)

    //Retour au Menu
    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }
}