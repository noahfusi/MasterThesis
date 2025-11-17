
import scala.io.StdIn.readLine
import java.io.PrintWriter
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.io.File

import java.io.PrintWriter
import scala.io.Source

//Déclaration de la class Machine
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  // Déclaration ensuite des 2 méthodes pour la class Machine

  //1. La méthode addIngredient
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "sugar") {
      sugar = sugar + amount
    } else if (ingredient == "coffee") {
      coffee = coffee + amount
    } else if (ingredient == "milk") {
      milk = milk + amount
    }
  }
  //2. la méthode removeIngredient
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "sugar" && sugar >= amount) {
      sugar = sugar - amount
      return true

    } else if (ingredient == "coffee" && coffee >= amount) {
      coffee = coffee - amount
      return true

    } else if (ingredient == "milk" && milk >= amount) {
      milk = milk - amount
      return true

    } else {
      return false
    }
  }


}

object Main {
  var casnum = 0
  var machineId = 0
  var action = 0


  var prix = 0.0


  def main(args: Array[String]): Unit = {
    val file = "machines.csv"
    val machines = loadcsv(file)
    println("\nMachines chargées :")
    var i = 0
    while (i < machines.length) {
      val machine = machines(i)
      println(s"Machine ${machine.id} chargée :")
      println(f"\tID: ${machine.id}")
      println(f"\tCode PIN: ${machine.pincode}")
      println(f"\tLait: ${machine.milk.toDouble / 1000}%.3fL")
      println(f"\tSucre: ${machine.sugar}g")
      println(f"\tCafé: ${machine.coffee}g")
      println()
      i += 1
    }

    while(casnum!=3){
      casnum = readLine("Nospresso Café\nSélectionnez votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toInt
      while ((casnum < 1) || (casnum > 3)) {
        casnum = readLine("Nospresso Café\nSélectionnez votre mode :\n1) Client\n2) Admin\n3) Quitter\n>").toInt}

      if (casnum==1){
        machineId = readLine(s"Machine sélectionnée ( 1-${machines.length} )\n>").toInt
        while ((machineId < 1) || (machineId > machines.length)){
          machineId = readLine(s"Machine sélectionnée ( 1-${machines.length} )\n>").toInt}
        machineId -= 1 //Afin de trouver le bon Index dans les tableaux

        serveClient(machines(machineId))
      }

      if(casnum==2){
        machineId = readLine(s"Machine sélectionnée ( 1-${machines.length} )\n>").toInt
        while ((machineId < 1) || (machineId > machines.length)){
          machineId = readLine(s"Machine sélectionnée ( 1-${machines.length} )\n>").toInt}
        machineId -= 1

        if(validatePin(machines(machineId))){
          action = readLine("1) Réapprovisionner les stocks\n2) Changer le code pin \n>").toInt
          while (action<1||action>2){
            action = readLine("1) Réapprovisionner les stocks\n2) Changer le code pin \n>").toInt}
          if(action==1){restockMachine(machines(machineId))}
          if(action==2){updatePin(machines(machineId))}
        }else{
          casnum=3//si le code pin n'est pas valide au bout de trois essai le programme s'arrête
        }
      }
    }
    savecsv(file, machines)
    println("Vous quittez le programme.")
  }
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val all = Source.fromFile(filename)
      val lignes = all.getLines()
      lignes.next()

      while (lignes.hasNext) {
        val ligne = lignes.next()
        val elements = ligne.split(",")
        if (elements.length == 4) {
          machines += new Machine(machines.length + 1, elements(0), elements(1).toInt, elements(2).toInt, elements(3).toInt
          )
        }
      }
    } catch {
      case e: Exception =>
        println("Erreur : Impossible de lire le fichier. Vérifiez le chemin du fichier ou bien le format du fichier.")
        sys.exit(1)
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")

      var i = 0
      while (i < machines.length) {
        val machine = machines(i)
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
        i = i + 1
      }

      writer.close()
      println(s"Le sauvegardement des machines a été fait dans $filename.")
    } catch {
      case e: java.io.IOException =>
        println(s"Erreur : Impossible d'écrire dans le fichier $filename. Le fichier peut être verrouillé ou en lecture seule.")
        sys.exit(1)
      case e: Exception =>
        println("Erreur : Échec de la sauvegarde des machines.")
        sys.exit(1)
    }
  }


  var Pin_tentative = " "

  def validatePin(machine : Machine): Boolean = {

    Pin_tentative = readLine("Saisissez le code PIN >")
    if(Pin_tentative!= machine.pincode){
      println("Code PIN incorrect.")
      println("2 tentatives restantes")
      Pin_tentative = readLine("Saisissez le code PIN >")
    }
    if(Pin_tentative!= machine.pincode){
      println("Code PIN incorrect.")
      println("1 tentatives restantes")
      Pin_tentative = readLine("Saisissez le code PIN >")
    }
    if(Pin_tentative!= machine.pincode){
      println("trop de tentatives échouées")
      return false
    }
    else if (Pin_tentative == machine.pincode) {
      println("Accès accordé à la Machine " + (machine.id +1))
      return true
    }
    false
  }

  var PIN_Nouv = " "


  def updatePin(machine : Machine): Unit = {

    println("Mise à jour du code PIN pour la Machine " + (machine.id+1))
    PIN_Nouv = readLine("Veuillez entrer un nouveau PIN de 6 chiffres\n>")

    while ( (PIN_Nouv.length > 6 || PIN_Nouv.length < 6 ) || (!PIN_Nouv.forall(_.isDigit)) ){
      println("le PIN doit contenir strictement 6 chiffres")
      PIN_Nouv = readLine("Veuillez entrer un nouveau PIN de 6 chiffres\n>")
    }
    println("PIN mis à jour\nRetour au menu principal...")
    machine.pincode = PIN_Nouv
  }

  var provcafe = 0 //déclaration des variables, prov = (quantité) provisoire, sting = en lettres
  var provsucre = 0
  var provlait = 0
  var doselait= 0
  var boisson = 0
  var prixlait = 0.0
  var boissonstring = ""
  var sucrestring = ""
  var laitsupp = 0 //lait supplémentaire
  var lattetaille=0

  def serveClient(machine : Machine): Boolean = {
    //menu client
    boisson = readLine("1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
    while (boisson<1 || boisson>3) { //choix de la boisson
      boisson = readLine("1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
    }
    if(boisson==1){ //cas expresso
      boissonstring = "Expresso"
      provcafe += 8
      prix += 2
    }
    if(boisson == 2){ //Cas cappuccino
      boissonstring = "Cappuccino"
      provcafe += 6
      provlait += 100
      prix += 2.5
    }
    if(boisson == 3){ //Cas latte
      lattetaille = readLine("Quelle taille pour votre Latte ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n>").toInt
      while(lattetaille<1 || lattetaille>3){ //on demande la taille du latte et on fait attention aux mauvais inputs
        lattetaille = readLine("Quelle taille pour votre Latte ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70\n>").toInt
      }
      boissonstring = "Latte (Petit)" //cas de base du latte
      prix += 2.70 + (lattetaille-1)*0.5
      provcafe += 6
      provlait += 120
      if(lattetaille==2){ // si taille == moyen
        boissonstring = "Latte (Moyen)"
        provcafe += 2
        provlait += 30
      }
      if(lattetaille==3){// si taille == grand
        boissonstring = "Latte (Grand)"
        provcafe += 6
        provlait += 80
      }
    }//déclaration pour le sucre:
    var sucre = readLine("Souhaitez-vous ajouter du sucre ? \n1) Sans sucre  \n2) Peu (5g) - CHF 0.10 \n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>").toInt
    while (sucre<1 || sucre>4){sucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>").toInt}
    provsucre = sucre*5 - 5 //la quantité de sucre est décrite par cette fonction
    if(sucre == 1){
      sucrestring = "Sans sucre"
    }
    if(sucre == 2){
      sucrestring = "Peu (5g)"
    }
    if(sucre == 3){
      sucrestring = "Moyen (10g)"
    }
    if(sucre == 4){
      sucrestring = "Beaucoup (15g)"
    }
    if (boisson == 2 || boisson == 3){ //cas pour le lait en +
      laitsupp = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n>").toInt
      while(laitsupp<1 || laitsupp>2){
        laitsupp = readLine("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n1) Oui\n2) Non\n>").toInt}
      if(laitsupp==1){ // si on veut du lait
        while(doselait<1 || doselait>3){ //on choisis la dose
          doselait = readLine("Combien de doses ? (max 3) \n>").toInt
        }
        provlait += 50*doselait //la quantité de lait en + se rajoute
        prixlait = doselait*0.05 //le prix du lait supplémentaire
      }
    }
    println(s"Boisson sélectionnée : $boissonstring") //on print le texte avec les string de la boisson et de la quantité de sucre
    println(s"Niveau de sucre : $sucrestring")
    if((boisson == 2 || boisson ==3 ) && laitsupp == 1){ //cas ou il y a du lait en +
      println( s"Lait en supplément: Oui, $doselait dose(s)" )
    }
    else if((boisson == 2||boisson == 3) && laitsupp == 2){ // cas ou il n'y a pas de lait
      println( "Lait en supplément: Non" )
    }
    if(provcafe>machine.coffee){ //cas erreure pas assez de café
      println("Erreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionn´ee.\nVeuillez choisir une autre machine")
      provlait = 0
      provsucre = 0
      provcafe = 0
    }
    else if(provsucre > machine.sugar){ //cas erreur sucre
      println("Erreur : Quantité de poudre de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre machine.")
      provlait = 0
      provsucre = 0
      provcafe = 0
    }
    else if(provlait>machine.milk && boisson == 3){ //cas erreur lait si la boisson est un latte
      print(provlait)
      println("Erreur : Quantité de lait insuffisante pour préparer\nla boisson sélectionnée.\nVeuillez choisir une autre machine")
      provlait = 0
      provsucre = 0
      provcafe = 0
    }
    else if(provlait>machine.milk && boisson != 3){//cas erreur lait si c'est pas un latte
      println("Erreur : Quantité de lait insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
      provlait = 0
      provsucre = 0
      provcafe = 0
    }
    else{ //si il n'y a pas d'erreures
      var prixsucre = (sucre-1)*0.1 //calcul prix sucre
      var prixtot = prix +prixsucre+prixlait
      println(f"Prix total : CHF $prix%.2f + CHF $prixsucre%.2f + CHF $prixlait%.2f = CHF $prixtot%.2f")
      val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" //on déclare tout les chars qui peuvent être utiliser pour le code twint
      var codepayement =( //déclaration du code random a 5 charactères
        chars(Random.nextInt(chars.length)).toString +
          chars(Random.nextInt(chars.length)).toString +
          chars(Random.nextInt(chars.length)).toString+
          chars(Random.nextInt(chars.length)).toString +
          chars(Random.nextInt(chars.length)).toString
        )
      println(s"Veuillez payer en utilisant Twint.\nVotre code de paiement est : $codepayement\n(En attente de validation du paiement...)") //on print le code
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.")
      println("préparation de votre boisson...")
      Thread.sleep(5000)
      println(s"Votre $boissonstring est prêt ! Bonne dégustation !")
      machine.removeIngredient("coffee", provcafe)
      machine.removeIngredient("milk", provlait)
      machine.removeIngredient("sugar", provsucre)

      return true
    }
    provcafe = 0 // on reset les valeures propres à ce cas précis
    provlait = 0
    provsucre = 0
    doselait = 0
    laitsupp = 0
    boisson = 0
    boissonstring = ""
    prix = 0
    sucrestring = ""
    false
  }

  def restockMachine(machine : Machine): Unit = {

    println("Mode Admin\nStocks:\n\tPoudre de café: " + machine.coffee + " g\n\tLait : " + machine.milk + " ml\n\tSucre : " + machine.sugar + " g\nRéapprovisionnement des stocks...\nAjout :")
    //on ajoute un par un les valeurs que l'utilisateur rentre
    var cafeplus = readLine("Poudre de cafe:").toInt
    while (cafeplus<0){cafeplus=readLine("Entrez une valeure positive. Poudre de cafe:").toInt}//condition pour que la valeur entrée soit strictemetn positive
    machine.addIngredient("coffee", cafeplus)
    var laitplus = readLine("Lait :").toInt
    while (laitplus<0) {laitplus = readLine("Entrez une valeure positive. Lait :").toInt}
    machine.addIngredient("milk", laitplus)
    var sucreplus = readLine("Sucre :").toInt
    while (sucreplus<0){sucreplus = readLine("Entrez une valeure positive. Sucre :").toInt}
    machine.addIngredient("sugar", sucreplus)

    println("Niveaux de stock mis à jour.\nRetour au menu principal...")
  }

}