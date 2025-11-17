import java.io.{BufferedWriter, File, FileWriter, IOException}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn._
import scala.util.Random

// Classe Machine
class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = ingredient.toLowerCase match {
    case "milk" => milk += amount
    case "sugar" => sugar += amount
    case "coffee" => coffee += amount
    case _ => println(s"Ingrédient non valide : $ingredient")
  }

  def removeIngredient(ingredient: String, amount: Int): Boolean = ingredient.toLowerCase match {
    case "milk" if milk >= amount => milk -= amount; true
    case "sugar" if sugar >= amount => sugar -= amount; true
    case "coffee" if coffee >= amount => coffee -= amount; true
    case _ => println(s"Stock insuffisant pour $ingredient."); false
  }
}

object Nospresso {
  val random = new Random()
  val lettreetnumero = ('A' to 'Z') ++ ('0' to '9')

  var machines: ArrayBuffer[Machine] = ArrayBuffer()

  def main(args: Array[String]): Unit = {

    machines = loadcsv("machines.csv")
    machines.zipWithIndex.foreach { case (machine, idx) =>
      println(s"\nMachine ${idx + 1} chargée :\n    ID: ${machine.id}\n    code PIN: ${machine.pincode}\n    Lait: ${machine.milk / 1000.0}L\n    Sucre: ${machine.sugar}g\n    Café: ${machine.coffee}g")
    }
    println(s"\n${machines.size} machine(s) chargée(s) avec succès.")

    var lancer = true
    while (lancer) {
      println("\n\n        Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter")
      readIntOption("> ") match {
        case Some(1) => modeClient(machines)
        case Some(2) => modeAdmin(machines)
        case Some(3) =>
          lancer = false
        case _ => println("Choix invalide. Veuillez réessayer.")
      }
    }

    savecsv("machines.csv", machines)
  }


  // partie traitant les fichiers
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    println("\nChargement des machines depuis machines.csv ...")
    val machines = ArrayBuffer[Machine]()
    try {
      val source = Source.fromFile(filename)
      val lines = source.getLines().toList
      source.close()

      if (lines.nonEmpty) {
        for ((line, idx) <- lines.tail.zipWithIndex) {
          val data = line.split(",").map(_.trim)
          if (data.length == 4) {
            val machine = new Machine(
              id = idx + 1,
              pincode = data(0),
              milk = data(1).toInt,
              sugar = data(2).toInt,
              coffee = data(3).toInt
            )
            machines.append(machine)
          } else {
            println(s"\nErreur : Ligne ${idx + 2} du fichier CSV mal formée.")
          }
        }
      }
    } catch {
      case _: java.io.FileNotFoundException =>
        println("\nErreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        println("\nErreur : Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
        System.exit(1)
      case ex: Exception =>
        println(s"\nErreur :  Echec du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
        System.exit(1)
    }
    machines
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val file = new File(filename)
      val writer = new BufferedWriter(new FileWriter(file))

      writer.write("PINCODE,MILK,SUGAR,COFFEE\n")
      for (machine <- machines) {
        writer.write(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}\n")
      }
      writer.close()
      println(s"\nSauvegarde de ${machines.length} machines dans machine.csv...\nFichier sauvegardé avec succès.")
    } catch {
      case ex: IOException =>
        println(s"\nErreur : Echec de l’écriture dans machines.csv.\nLe fichier peut être verrouillé ou en lecture seule.")
        println(s"\nErreur: Eche du chargement ou de la sauvegarde des machines.\nFermeture du programme.")
        System.exit(1)
    }
  }
  //fin de la partie traitant les fichiers


  // Mode Client
  def modeClient(machines: ArrayBuffer[Machine]): Unit = {
    println("Mode Client - Commandez votre boisson")
    var machineId = demanderChoix(s"\nSélectionnez une machine (1-${machines.size}) :", 1, machines.size) - 1
    println(s"Machine ${machineId + 1} sélectionnée.")

    var continuer = true
    while (continuer) {
      println("\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n4) Retour")
      readIntOption("> ") match {
        case Some(1) =>
          if (verifierStock(machines ,machineId, 8, 0, 0.0)) {
            traiterBoisson(machines,machineId, "Expresso", 8, 0.0, 2.00, sucreInclus = true, laitInclus = false)
            continuer = false
          } else {
            println("Erreur : Stock insuffisant pour préparer un Expresso.")
            machineId = demanderChoix(s"Choisissez une autre machine (1-${machines.size}) :", 1, machines.size) - 1
          }
        case Some(2) =>
          if (gererCappuccino(machines,machineId)) continuer = false
          else {
            machineId = demanderChoix(s"Choisissez une autre machine (1-${machines.size}) :", 1, machines.size) - 1
          }
        case Some(3) =>
          if (gererLatte(machines,machineId)) continuer = false
          else {
            machineId = demanderChoix(s"Choisissez une autre machine (1-${machines.size}) :", 1, machines.size) - 1
          }
        case Some(4) =>
          println("Retour au menu principal.")
          continuer = false
        case _ => println("Choix invalide.")
      }
    }
  }

  def verifierStock(machines: ArrayBuffer[Machine] ,id: Int, cafe: Int, sucre: Int, lait: Double): Boolean =
    machines(id).coffee >= cafe && machines(id).sugar >= sucre && machines(id).milk >= lait

  def traiterBoisson(machines: ArrayBuffer[Machine] ,machineId: Int, nom: String, cafeNecessaire: Int, laitNecessaire: Double, prixBase: Double, sucreInclus: Boolean, laitInclus: Boolean): Unit = {
    val (niveauSucre,sucreNecessaire, prixSucre) = if (sucreInclus) choisirSucre() else ("",0, 0.0)

    if (verifierStock(machines, machineId, cafeNecessaire, sucreNecessaire, 0)) {
      machines(machineId).removeIngredient("coffee", cafeNecessaire)
      machines(machineId).removeIngredient("sugar", sucreNecessaire)

      val prixTotal = prixBase + prixSucre + 0
      afficherRecapitulatif(nom, prixBase, prixSucre, niveauSucre,0,prixTotal,0,0)
      effectuerPaiement()
      preparerBoisson(nom)
    } else {
      println(s"Erreur : Stock insuffisant pour préparer votre $nom. Choisissez une autre boisson.")
    }
  }

  def gererCappuccino(machines: ArrayBuffer[Machine],machineId: Int): Boolean = {
    val (niveauSucre,sucreNecessaire, prixSucre) = choisirSucre()
    val (laitSupplementaire,prixSupplementLait) = choisirSupplementLait()

    if (verifierStock(machines,machineId, 6, sucreNecessaire, 100 + laitSupplementaire)) {
      machines(machineId).removeIngredient("coffee", 6)
      machines(machineId).removeIngredient("sugar", sucreNecessaire)
      machines(machineId).removeIngredient("milk", 100+laitSupplementaire)
      val prixTotal = 2.50 + prixSucre + prixSupplementLait
      afficherRecapitulatif("Cappuccino", 2.50, prixSucre,niveauSucre, prixSupplementLait, prixTotal,laitSupplementaire,prixSupplementLait)
      effectuerPaiement()
      preparerBoisson("Cappuccino")
      true
    } else {
      println("Erreur : Stock insuffisant.")
      false
    }
  }


  def gererLatte(machines: ArrayBuffer[Machine], machineId: Int): Boolean = {
    val tailleLatte = demanderChoix("Quelle taille de Latte ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70", 1, 3)
    val (cafeRequis, laitRequis, prixBase) = tailleLatte match {
      case 1 => (6, 120, 2.70)
      case 2 => (8, 150, 3.20)
      case 3 => (12, 200, 3.70)
    }

    val (niveauSucre,sucreNecessaire, prixSucre) = choisirSucre()
    val (laitSupplementaire,prixSupplementLait) = choisirSupplementLait()

    if (verifierStock(machines,machineId, cafeRequis, sucreNecessaire, laitRequis + prixSupplementLait)) {
      machines(machineId).removeIngredient("coffee", cafeRequis)
      machines(machineId).removeIngredient("sugar", sucreNecessaire)
      machines(machineId).removeIngredient("milk", laitRequis+laitSupplementaire)
      val prixTotal = prixBase + prixSucre + prixSupplementLait
      afficherRecapitulatif("Latte", prixBase, prixSucre, niveauSucre,prixSupplementLait, prixTotal,laitSupplementaire,prixSupplementLait)
      effectuerPaiement()
      preparerBoisson("Latte")
      true
    } else {
      println("Erreur : Stock insuffisant.")
      false
    }
  }

  def choisirSucre(): (String,Int, Double) = {
    println("Souhaitez vous ajouter du sucre ?")
    val niveau = demanderChoix("1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30", 1, 4)
    niveau match { case 1 => ("Sans sucre",0, 0.0); case 2 => ("Peu (5g)",5, 0.10); case 3 => ("Moyen (10)g",10, 0.20); case 4 => ("Beaucoup (15g)",15, 0.30) }
  }


  def choisirSupplementLait(): (Int, Double)= {
    val choix = demanderChoix("Souhaiter-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappucino et Latte)\n1) Oui\n2) Non", 1, 2)
    if (choix == 1) demanderChoix("\nCombien de dose (1,2,3 max)?", 1, 3) match {
      case 1 => (50, 0.05); case 2 => (100, 0.10); case 3 => (150,0.15)
    } else (0, 0.0)
  }


  def afficherRecapitulatif(nom: String, base: Double, prixSucre: Double, niveauSucre : String, lait: Double, total: Double, quantiteLaitSupp: Int, prixLaitSupp: Double): Unit = {
    println(f"\nBoisson sélectionné : $nom")
    if (nom == "Expresso" && prixSucre==0) {
      println(s"Niveau de sucre : ${niveauSucre}")
      println(f"Prix Total: CHF $base%.2f")
    }
    else if (nom== "Expresso" && prixSucre>0){
      println(s"Niveau de sucre : ${niveauSucre}")
      println(f"Prix Total: CHF $base%.2f + CHF $prixSucre%.2f = CHF $total%.2f")
    }

    else { //sinon c'est la cappuccino ou Latte
      if(prixSucre==0 && prixLaitSupp ==0){
        println(s"Niveau de sucre : ${niveauSucre}")
        println(s"Lait Supplémentaire : Non")
        println(f"Prix Total: CHF $base%.2f")
      }
      else if(prixSucre>0 && prixLaitSupp ==0){
        println(s"Niveau de sucre : ${niveauSucre}")
        println(s"Lait Supplémentaire : Non")
        println(f"Prix Total: CHF $base%.2f + CHF $prixSucre%.2f = CHF $total%.2f")
      }
      else if (prixSucre==0 && prixLaitSupp>0){
        println(s"Niveau de sucre : ${niveauSucre}")
        println(s"Lait Supplémentaire : Oui")
        println(f"Prix Total: CHF $base%.2f + CHF $prixLaitSupp%.2f = CHF $total%.2f")
      }
      else{
        println(s"Niveau de sucre : ${niveauSucre}")
        println(s"Lait Supplémentaire : Oui")
        println(f"Prix Total: CHF $base%.2f + CHF $prixSucre%.2f + CHF $prixLaitSupp%.2f = CHF $total%.2f")
      }
    }


  }

  def effectuerPaiement(): Unit = {
    val code = (1 to 5).map(_ => lettreetnumero(random.nextInt(lettreetnumero.length))).mkString
    println(s"\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : $code")
    println("(En attente de paiement...)")
    Thread.sleep(3000)
    println("\nPaiement confirmé. Merci !")
  }

  def preparerBoisson(nom: String): Unit = {
    println(s"Préparation de votre Boisson...")
    Thread.sleep(2000)
    println(s"Votre $nom est prêt ! Bonne dégustation !")
  }



  // fin des fonctions gérant le mode client


  // Mode Admin
  def modeAdmin(machines: ArrayBuffer[Machine]): Unit = {
    val machineId = demanderChoix(s"Sélectionnez une machine (1-${machines.size}) :", 1, machines.size) - 1
    if (validatePin(machines, machineId)) {
      println(s"Accès accordé à la Machine ${machineId + 1}.")
      println("1) Réapprovisionner\n2) Mettre à jour le code PIN\n3) Retour")
      readIntOption("> ") match {
        case Some(1) => restockMachine(machines,machineId)
        case Some(2) => updatePin(machines,machineId)
        case Some(3) => println("Retour au menu principal...")
        case _ => println("Choix invalide.")
      }
    } else println("Accès refusé. Retour au menu principal.")
  }


  def validatePin(machines: ArrayBuffer[Machine],machineId: Int): Boolean = {
    var tentatives = 3
    while (tentatives > 0) {
      println(s"Entrez le code PIN de la Machine ${machineId + 1}:")
      if (readLine() == machines(machineId).pincode) return true
      tentatives -= 1
      println(s"PIN incorrect. ${tentatives} tentative(s) restante(s).")
    }
    println("Trop de tentatives échouées. Le programme va se fermer.")
    System.exit(0)  // Arrêt complet du programme
    false // Cette ligne ne sera jamais atteinte mais elle est nécessaire pour la méthode
  }


  def updatePin(machines: ArrayBuffer[Machine],machineId: Int): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres :")
    val newPin = readLine()
    if (newPin.matches("\\d{6}")) {
      machines(machineId).pincode = newPin
      println("Code PIN mis à jour avec succès.\nRetour au menu principal...")
    } else println("Le code PIN n’a pas été mis à jour.\nRetour au menu principal...")
  }

  def restockMachine(machines: ArrayBuffer[Machine] ,machineId: Int): Unit = {
    println(s"Niveaux de stocks actuels : \n   Poudre de café : ${machines(machineId).coffee}g\n   Sucre : ${machines(machineId).sugar}g\n   Lait : ${machines(machineId).milk / 1000.0}L")
    println("\nEntrez les quantités à ajouter :")
    machines(machineId).addIngredient("coffee", demanderQuantite("Poudre de café (g) :"))
    machines(machineId).addIngredient("sugar", demanderQuantite("Sucre (g) :"))
    machines(machineId).addIngredient("milk", (demanderQuantiteDouble("Lait (L) :")*1000).toInt)
    println("Stocks mis à jour avec succès.\nRetour au menu principal...")
  }
  // fin des fonctions gérant le mode admin


  // Fonctions Utilitaires

  def demanderChoix(message: String, min: Int, max: Int): Int = {
    var choix = 0
    do {
      println(message)
      choix = readIntOption("> ").getOrElse(0)
    } while (choix < min || choix > max)
    choix
  }

  def demanderQuantite(msg: String): Int = { print(msg); readIntOption("> ").getOrElse(0).max(0) }
  def demanderQuantiteDouble(msg: String): Double = { print(msg); readDoubleOption("> ").getOrElse(0.0).max(0.0) }
  def readIntOption(prompt: String): Option[Int] = { print(prompt); try Some(readLine().toInt) catch { case _: Exception => None } }
  def readDoubleOption(prompt: String): Option[Double] = { print(prompt); try Some(readLine().toDouble) catch { case _: Exception => None } }
}
