import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn._
import scala.util.Random
import java.io.File
import java.io.PrintWriter
import scala.io.Source


class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "milk") {
      milk = milk + amount
    } else if (ingredient == "coffee") {
      coffee = coffee + amount
    } else if (ingredient == "sugar") {
      sugar = sugar + amount
    }
    else{
      println("Vous avez mis un ingrédient inconnu ")
    }
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "sugar") {
      if (sugar >= amount) {
        sugar -= amount
        true
      } else {

        false
      }
    } else if (ingredient == "coffee") {
      if (coffee >= amount) {
        coffee -= amount
        true
      } else {

        false
      }
    } else if (ingredient == "milk") {
      if (milk >= amount) {
        milk -= amount
        true
      } else {

        false
      }
    } else {

      false
    }
  }


}

object Main {
  var machineId = -1
  var machinePins = Array.fill(5)("434343")
  var Iteration = 0
  var Qcafe = Array.fill(5)(50)
  var Qsucre = Array.fill(5)(30)
  var Qlait = Array.fill(5)(500)

  var PlusCafe = 0
  var PlusSucre = 0
  var PlusLait = 0

  var Latiteration = 0
  var AjSucre = 0
  var status = 0
  var ChoixDuCafe = 0
  var TypeDeBoisson = ""
  var DLait = ""
  var nDLait = 0

  var StringSucre = ""

  var Prix:Double = 0
  var PSucre: Double = 0
  var PLait:Double = 0
  var ILatte = 0
  var PrixFinal = 0.0

  var UseCafe = 0
  var UseSucre = 0
  var UseLait = 0


  var CodePourTWINT = ""
  var EntreeCode = ""
  var NouveauCode = ""
  var validationPIN = false
  var ChoixAdmin = 0

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val file = Source.fromFile(filename)
      val lignes = file.getLines()

      if (lignes.hasNext){
        lignes.next()
      }

      var i = 1
      while (lignes.hasNext) {
        val ligneActuelle = lignes.next()
        val champs = ligneActuelle.split(",")
        if (champs.length == 4) {
          val nom = champs(0)
          val valeur1 = champs(1).toInt
          val valeur2 = champs(2).toInt
          val valeur3 = champs(3).toInt

          machines.append(new Machine(i, nom, valeur1, valeur2, valeur3))
          i += 1
        }
      }
    } catch {
      case e: Exception =>
        println(s"Erreur : Le fichier $filename est introuvable (impossible de le lire).")
        sys.exit(1)

    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(filename)
      writer.println("PINCODE,MILK,SUGAR,COFFEE")

      var i = 0
      while (i < machines.size) {
        val currentMachine = machines(i)
        writer.println(s"${currentMachine.pincode},${currentMachine.milk},${currentMachine.sugar},${currentMachine.coffee}")
        i += 1
      }

      writer.close()
      println(s"Les données des machines ont été sauvegardées avec succès dans le fichier : $filename.")
    } catch {
      case e: java.io.IOException =>
        println(s"Erreur : Impossible d'écrire dans le fichier $filename. Le fichier peut être verrouillé ou en lecture seule.")
        sys.exit(1)
      case e: Exception =>
        println("Erreur inattendue : Échec du sauvegardement des machines.")
        sys.exit(1)
    }
  }


  def updatePin(machine : Machine): Unit = {

    println("modification du code PIN de la machine " + (machine.id))
    println("Entrez le Nouveau code PIN")
    NouveauCode = readLine()
    if(NouveauCode.forall(_.isDigit) && (NouveauCode.length == 6)){
      validationPIN=true
    }else {validationPIN=false}
    while(!validationPIN){
      println("Erreur le code PIN doit être composer de 6 chiffres")
      println("Entrez le Nouveau code PIN")
      NouveauCode = readLine()
      if(NouveauCode.forall(_.isDigit) && (NouveauCode.length == 6)){
        validationPIN=true
      }else{validationPIN=false}
    }

    machine.pincode = NouveauCode
    println("le code PIN a bien été mis à jour sur la machine"+(machineId+1))
  }


  def validatePin(machine : Machine): Boolean = {
    EntreeCode = readLine("Entrez le code PIN >")
    if (EntreeCode == machine.pincode) {
      println("Accès accordé à la Machine " + machine.id)
      return true
    } else if(machine.pincode !=EntreeCode) {
      println("Code PIN incorrect : 2 tentatives restantes")
      EntreeCode = readLine("Entrez le code PIN >")
    }

    if (EntreeCode == machine.pincode) {
      println("Accès accordé à la Machine " + machine.id)
      return true
    } else if(machine.pincode!=EntreeCode) {
      println("Code PIN incorrect : 1 tentatives restantes")
      EntreeCode = readLine("Entrez le code PIN >")
    }
    if (EntreeCode == machine.pincode) {
      println("Accès accordé à la Machine " + (machine.id))
      return true
    }else if(machine.pincode!=EntreeCode) {
      println("Code PIN incorrect : 0 tentatives restantes")
    }
    println("Trop de tentatives échouées. Fin du programme.")
    false
  }


  def restockMachine(machine : Machine): Unit = {

    println("Mode Admin")
    // Montrer le stock puis le mettre a jour avec montants perso - /Affichage en Litres mais variable stock de lait en mL/
    println("Stocks:Poudre de cafe: "+  machine.coffee +" g    \nLait          : "+ machine.milk +" ml   \nSucre         :   "+ machine.sugar +" g\n")
    println("Reapprovisionnement des stocks...\n")

    println("Quantité de poudre à ajouter : ")
    PlusCafe = readInt()
    while(PlusCafe<0){//validation des entrées positive
      println("erreur entrez un nombre positif")
      PlusCafe = readInt()
    }
    println("Quantité de Sucre à ajouter : ")
    PlusSucre = readInt()
    while(PlusSucre<0){
      println("erreur entrez un nombre positif")
      PlusSucre = readInt()
    }
    println("Quantité de lait à ajouter (en ml) : ")
    PlusLait = readInt()
    while(PlusLait<0){
      println("erreur entrez un nombre positif")
      PlusLait = readInt()
    }
    println("Ajout :")
    println("Poudre de cafe : " + PlusCafe + " g")
    println("Lait           : " + PlusLait + " ml")
    println("Sucre          : " + PlusSucre + " g")
    machine.addIngredient("coffee", PlusCafe)
    machine.addIngredient("sugar", PlusSucre)
    machine.addIngredient("milk", PlusLait)

    println("Niveaux de stock mis a jour. ")
    println("Retour au menu principal.    ")
  }

  def serveClient(machine : Machine): Boolean = {
    // Mode Client

    println("Veuillez selectionner votre boisson :")
    println("1) Expresso - CHF 2.00")
    println("2) Cappuccino - CHF 2.50")
    println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")
    while(ChoixDuCafe < 1 || ChoixDuCafe > 3){ // Verification des entrées
      ChoixDuCafe = readInt()
      if(ChoixDuCafe < 1 || ChoixDuCafe > 3){
        println("Entrée incorrecte.")
      }
    }

    if(ChoixDuCafe == 1){ // Traitement du choix de la boisson
      TypeDeBoisson = "Expresso"
      UseCafe = 8
      Prix = 2
    } else if (ChoixDuCafe == 2){
      TypeDeBoisson = "Capuccino"
      Prix = 2.5
      UseCafe = 6
      UseLait = 100
    } else if (ChoixDuCafe == 3){
      ILatte = 0
      println("Quelle taille ? ")
      println("1) Petit - CHF 2.70 ")
      println("2) Moyen - CHF 3.20")
      println("3) Grand - CHF 3.70 ")
      print("> ")
      while(ILatte < 1 || ILatte > 3){
        ILatte = readInt()
        if(ILatte < 1 || ILatte > 3){
          println("Entrée incorrecte.")
        }
      }

      if(ILatte == 1){ // Traitement taille du Latte
        TypeDeBoisson = "Latte (petit)"
        Prix = 2.7
        UseCafe = 6
        UseLait = 120
      } else if (ILatte == 2){
        TypeDeBoisson = "Latte (moyen)"
        Prix = 3.2
        UseCafe = 8
        UseLait = 150
      } else if (ILatte == 3){
        TypeDeBoisson = "Latte (grand)"
        Prix = 3.7
        UseCafe = 12
        UseLait = 200
      }
    }

    println("Souhaitez-vous ajouter du sucre ?   ") // Traitement du sucre
    println("1) Sans sucre                 ")
    println("2) Peu (5g) - CHF 0.10        ")
    println("3) Moyen (10g) - CHF 0.20     ")
    println("4) Beaucoup (15g) - CHF 0.30  ")
    print("> ")
    while(AjSucre < 1 || AjSucre > 4){ // Verification des entrées
      AjSucre = readInt()
      if(AjSucre < 1 || AjSucre > 4){
        println("Entrée incorrecte.")
      }
    }
    if (AjSucre == 1) {
      StringSucre = "Sans sucre"
      UseSucre = 0
      PSucre = 0
    } else if (AjSucre == 2) {
      StringSucre = "Peu (5g)"
      UseSucre = 5
      PSucre = 0.1
    } else if (AjSucre == 3) {
      StringSucre = "Moyen (10g)"
      UseSucre = 10
      PSucre = 0.2
    } else if (AjSucre == 4) {
      StringSucre = "Beaucoup (15g)"
      UseSucre = 15
      PSucre = 0.3
    }

    if (TypeDeBoisson != "Expresso") { // Doses de lait
      println("Souhaitez-vous ajouter du lait en supplement ?      ")
      println("(Disponible uniquement pour Capuccino et Latte   ")
      println("1) Oui")
      println("2) Non")
      print("> ")
      while(Latiteration < 1 || Latiteration > 2){
        Latiteration = readInt()
        if(Latiteration < 1 || Latiteration > 2){
          println("Entrée invalide.")
        }
      }

      if (Latiteration == 1) {
        DLait = "Oui"
        println("Combien de dose ?    ")
        print("> ")
        nDLait = readInt()

        while (nDLait < 1 || nDLait > 3) { // Verification des entrées
          println("Veuillez selectionner une valeur correcte. ")
          print("> ")
          nDLait = readInt()
        }

        PLait = nDLait * 0.05
        UseLait += nDLait * 50

        nDLait = 0

      } else {
        DLait = "Non"
      }
    }

    status = 1
    //Verification s'il n'y a pas d'erreurs
    if(machine.coffee < UseCafe){
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson selectionnée.")
      status = 0
      PlusCafe = 0
      PlusSucre = 0
      PlusLait = 0
      ChoixDuCafe = 0
      Latiteration = 0
      AjSucre = 0
      status = 0
      nDLait = 0

      UseCafe = 0
      UseSucre = 0
      UseLait = 0

      Prix = 0
      PSucre = 0
      PLait = 0
    }
    if(machine.milk < UseLait){
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson selectionnée.")
      status = 0
      PlusCafe = 0
      PlusSucre = 0
      PlusLait = 0
      ChoixDuCafe = 0
      Latiteration = 0
      AjSucre = 0
      status = 0
      nDLait = 0

      UseCafe = 0
      UseSucre = 0
      UseLait = 0

      Prix = 0
      PSucre = 0
      PLait = 0
    }
    if(machine.sugar < UseSucre){
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson selectionnée.")
      status = 0
      PlusCafe = 0
      PlusSucre = 0
      PlusLait = 0
      ChoixDuCafe = 0
      Latiteration = 0
      AjSucre = 0
      status = 0
      nDLait = 0

      UseCafe = 0
      UseSucre = 0
      UseLait = 0

      Prix = 0
      PSucre = 0
      PLait = 0
    }

    if(status == 0){ // Erreur et variables à 0
      println("Veuillez choisir une autre machine.")
      PlusCafe = 0
      PlusSucre = 0
      PlusLait = 0
      ChoixDuCafe = 0
      Latiteration = 0
      AjSucre = 0
      status = 0
      nDLait = 0

      UseCafe = 0
      UseSucre = 0
      UseLait = 0

      Prix = 0
      PSucre = 0
      PLait = 0
    }

    if(status > 0){// Si il n'y a pas d'erreurs, passage au payement
      CodePourTWINT = Random.alphanumeric.take(5).mkString.toUpperCase()
      println("Boisson selectionnee : " + TypeDeBoisson)
      println("Niveau de sucre : " + StringSucre)
      if (TypeDeBoisson != "Expresso") {
        println("lait supplémentaire:" + DLait)
      }

      PrixFinal = Prix + PLait + PSucre
      print(f"Prix total : $Prix%.2f  + CHF $PSucre%.2f + CHF $PLait%.2f = $PrixFinal%.2f \n")
      println("")
      println ("Veuillez payer en utilisant TWINT.")
      println( "Votre code de payement est : " + CodePourTWINT)
      println( "En attente de payement... ")

      Thread.sleep(3000)

      println("Payement confirmé.")
      println("Préparation de votre boisson...")

      Thread.sleep(5000)
      machine.removeIngredient("coffee", UseCafe)
      machine.removeIngredient("sugar", UseSucre)
      machine.removeIngredient("milk", UseLait)


      println("Votre " + TypeDeBoisson + " est prêt ! Bonne dégustation !")

      //Remettre toutes les variables de reinitialisation
      PlusCafe = 0
      PlusSucre = 0
      PlusLait = 0
      ChoixDuCafe = 0
      Latiteration = 0
      AjSucre = 0
      status = 0
      nDLait = 0

      UseCafe = 0
      UseSucre = 0
      UseLait = 0

      Prix = 0
      PSucre = 0
      PLait = 0
      return true
    }
    false
  }

  def main(args: Array[String]): Unit = {
    val fichier = "machines.csv"
    val machines = loadcsv(fichier)
    for (machine <- machines) {
      println(s"Machine ${machine.id} chargée :")
      println(f"\tID: ${machine.id}")
      println(f"\tCode PIN: ${machine.pincode}")
      println(f"\tLait: ${machine.milk.toDouble / 1000}%.3fL")
      println(f"\tSucre: ${machine.sugar}g")
      println(f"\tCafé: ${machine.coffee}g")
      println()
    }
    while(Iteration!=3){
      println("     Nospresso Cafe")
      print("1) Client   \n")
      println("2) Admin    ")
      print("3) Quitter  \n")
      print("> ")
      Iteration = readInt()
      while(Iteration != 1 && Iteration != 2 && Iteration != 3){ // Verification des entrées
        Iteration = readInt()
        if(Iteration != 1 && Iteration != 2 && Iteration != 3){
          println("Entrée incorrecte.")
        }
      }

      if(Iteration==2){

        println(s"selection machine (1-${machines.length})")
        machineId = readInt()
        while(machineId<1||machineId>machines.length){
          machineId = readInt()}
        machineId = machineId - 1

        if(validatePin(machines(machineId))){//Demander le code pin en mode admin apres avoir choisi la machine

          println("Séelction du mode admin : \n1) Mettre à jour le code PIN\n2) Réapprovisionner les stocks")
          ChoixAdmin = readInt()
          while (ChoixAdmin<1||ChoixAdmin>2){
            println("Séelction du mode admin : \n1) Mettre à jour le code PIN\n2) Réapprovisionner les stocks")
            ChoixAdmin = readInt()
          }
          if (ChoixAdmin==1){
            updatePin(machines(machineId))}
          if(ChoixAdmin==2){
            restockMachine(machines(machineId))}

        }else{Iteration=3}//Si le code PIN est faux on sort de la boucle while(Iteration!=3)
      }

      if (Iteration==1){
        println(s"selection machine (1-${machines.length})")
        machineId = readInt()
        while(machineId<1||machineId>5){
          machineId = readInt()
        }
        machineId = machineId - 1
        serveClient(machines(machineId))
      }
    }
    savecsv(fichier, machines)
  }
}