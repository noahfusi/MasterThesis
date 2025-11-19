import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Random
import java.io.PrintWriter

object NospressoApp extends App {

 // définition de la classe Machine
 class Machine(val id: Int, var pincode : String, var milk: Int, var sugar: Int, var coffee: Int) {
  // Définition de la méthode addIngredient
  def addIngredient(ingredient: String, amount: Int): Unit = {
   ingredient match {
    case "milk" => milk += amount
    case "sugar" => sugar += amount
    case "coffee" => coffee += amount
    case _ => println("Erreur ingrédient inconnu")
   }
  }

  // Méthode validatePin
  def validatePin(id: Int, pincode: String): Boolean = {
   var tentatives = 3
   while (tentatives > 0) {
    var Pin = readLine("Entrez le code PIN : \n> ")
    if (Pin == pincode) {
     println("Accès accordé à la Machine " + (id) + ".")
     return true
    } else {
     tentatives -= 1
     if (tentatives > 0)
      println("Code PIN incorrect. " + tentatives + " tentatives restantes.")
    }
    if (tentatives == 0) {
     println("Code PIN incorrect. 0 tentatives restantes.\n\nTrop de tentatives échouées. Fin du programme.")
     return false
    }
   }
   false
  }

  // Méthode updatePin
  def updatePin(id: Int): Unit = {

   println("Mise à jour du code PIN pour la machine " + id)
   pincode = readLine("Entrez un nouveau code Pin à 6 chiffres > ")
   while (pincode.length != 6) {
    pincode = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
   }
   println("Le code PIN a été mis à jour avec succès.")
   println("Retour au menu principal...\n\n")
  }


  // Méthode restockMachine
  def restockMachine(id: Int): Unit = {
   println("Niveaux de stocks actuels :")
   println("Poudre de Café  : " + coffee + "g")
   println("Lait            : " + milk + "mL")
   println("Sucre           : " + sugar + "g")

   println("Entrez les quantités à ajouter : ")
   var ajoutcafe = readLine("Poudre de café > ").toInt
   var ajoutlait = readLine("Lait > ").toInt
   var ajoutsucre = readLine("Sucre > ").toInt
   addIngredient("coffee", ajoutcafe)
   addIngredient("milk", ajoutlait)
   addIngredient("sugar", ajoutsucre)
   println("Les stocks ont été mis à jour avec succès.")
   println("Retour au menu principal... \n")
   Thread.sleep(1500)
  }

  // définition de la méthode removeIngredient
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
   ingredient match {

    case "milk" => if (milk >= amount) {
     milk -= amount
     true
    } else {
     println("Erreur : Stock de lait insuffisant. Veuillez selectionner une autre machine.")
     false
    }
    case "sugar" => if (sugar >= amount) {
     sugar -= amount
     true
    } else {
     println("Erreur : Stock de sucre insuffisant. Veuillez selectionner une autre machine.")
     false
    }
    case "coffee" => if (coffee >= amount) {
     coffee -= amount
     true
    } else {
     println("Erreur : Stock de poudre de café insuffisant. Veuillez selectionner une autre machine.")
     false
    }
   }
  }
// Méthode serveClient définition
  def serveClient(id: Int): Boolean = {

   val expresso = 2.00
   val cappuccino = 2.50
   val latteP = 2.70
   val latteM = 3.20
   val latteG = 3.70
   var dose = 0
   var prix: Double = 0



   // Sélection de boissons
   println("Veuillez selectionner votre boisson : ")
   println("1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50 \n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
   var choixBoisson = readLine("> ").toInt
   while (choixBoisson < 1 || choixBoisson > 3) choixBoisson = readLine("Veuillez selectionner 1,2 ou 3 : ").toInt


   // Choix du sucre
   println("Souhaitez-vous ajouer du sucre ?")
   println("1) Sans sucre \n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
   var sucre = readLine("> ").toInt
   while (sucre < 1 || sucre > 4) sucre = readLine("Veuillez selectionner 1, 2, 3 ou 4 : ").toInt
   if (sucre == 2) {
    sugar -= 5
    prix += 0.10
   }
   if (sucre == 3) {
    sugar -= 10
    prix += 0.20
   }
   if (sucre == 4) {
    sugar -= 15
    prix += 0.30
   }


   // Choix du lait
   if (choixBoisson == 2 || choixBoisson == 3) {
    println("Souhaitez-vous ajouter du lait en supplément ?")
    println("1) Oui \n2) Non")
    var lait = readLine("> ").toInt
    while (lait < 1 || lait > 2) lait = readLine("Veuillez selectionner 1 ou 2 : ").toInt
    if (lait == 1) {
     println("1) 1 dose (50ml) - CHF 0.05 \n2) 2 doses (100ml) - CHF 0.10 \n3) 3 doses (150ml) - CHF 0.20")
     dose = readLine("> ").toInt
     while (dose < 1 || dose > 3) dose = readLine("Veuillez selectionner 1, 2, ou 3 : ").toInt
     if (dose == 1) {
      milk -= 50
      prix += 0.05
     }
     if (dose == 2) {
      milk -= 100
      prix += 0.10
     }
     if (dose == 3) {
      milk -= 150
      prix += 0.15
     }
    }
   }


   // Expresso
   if (choixBoisson == 1) {
    if (removeIngredient("coffee", 8) == true) {
     prix += expresso
    } else {
     return false
    }
   }

   // Cappuccino

   if (choixBoisson == 2) {
    if (removeIngredient("coffee", 6) == true && removeIngredient("milk", 100) == true) {
     prix += cappuccino
    } else {
     return false
    }
   }

   // Latte
   if (choixBoisson == 3) {
    println("Choissisez la taille de votre boisson : ")
    println("1) Petit \n2) Moyen \n3) Grand")
    var taille = readLine("Choisir 1, 2 ou 3 : ").toInt
    while (taille < 1 || taille > 3) taille = readLine("Veuillez selectionner 1, 2, ou 3 : \n> ").toInt
    if (taille == 1) {
     if (removeIngredient("coffee", 6) == true && removeIngredient("milk", 120) == true) {
      prix += latteP
     } else {
      return false
     }
    }
    if (taille == 2) {
     if (removeIngredient("coffee", 8) == true && removeIngredient("milk", 150) == true) {
      prix += latteM
     } else {
      return false
     }
    }
    if (taille == 3) {
     if (removeIngredient("coffee", 12) == true && removeIngredient("milk", 200) == true) {
      prix += latteG
     } else {
      return false
     }
    }
   }

   // Paiement
   printf("Le montant de votre commande est de : %.2f CHF\n\n", +prix)
   println("Veuillez payer en utilisant Twint.")
   val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
   var code = ""
   val longueurcode = 5
   for (_ <- 1 to longueurcode) {
    code += caracteres(Random.nextInt(caracteres.length))
   }
   println("Votre code de paiement est : " + code)
   println("(En attente de validation du paiement)\n")
   Thread.sleep(3000)
   println("Merci ! Votre paiement a été accepté.")
   Thread.sleep(1500)
   println("Préparation de votre boisson...")
   println("[...]")
   if (choixBoisson == 1) {
    println("Votre Expresso est prêt ! Bonne dégustation !\n\n")
   }
   if (choixBoisson == 2) {
    println("Votre Cappuccino est prêt ! Bonne dégustation !\n\n")
   }
   if (choixBoisson == 3) {
    println("Votre Latte est prêt ! Bonne dégustation !\n\n")
   }
   Thread.sleep(1500)
   return true
  }
 }

 // Définition de la méthode loadcsv
 def loadcsv(filename: String): ArrayBuffer[Machine] = {
  println("Chargement des machines depuis machines.csv...")
  val machines = ArrayBuffer[Machine]()
  try {
   val file = Source.fromFile(filename)
   for (line <- file.getLines().drop(1)) {
    val Array(pincode, milkStr, sugarStr, coffeeStr) = line.split(",")
    val milk = milkStr.toInt
    val sugar = sugarStr.toInt
    val coffee = coffeeStr.toInt
    val id = machines.size + 1
    val machine = new Machine(id, pincode, milk, sugar, coffee)
    machines += machine
    println("\nMachine " + id + " chargée : \nID : " + id + "\nCode PIN : " + pincode +
      "\nLait : " + milk + "\nSucre : " + sugar + "\nCafé : " + coffee)
    println("\n" + machines.size + " machine(s) chargée(s) avec succès.")
   }
   file.close()
  } catch {
   case e: Exception => println("Erreur : Fichier introuvable. Vérifiez le chemin d'accès et réessayez.")
  }
  machines
 }

 //définition de la méthode savecsv
 def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
  try {
   val writer = new PrintWriter(filename)
   writer.println("pincode,milk,sugar,coffee")
   for (machine <- machines) {
    val line = s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}"
    writer.println(line)
   }
   println("Sauvegarde de " + machines.size + " machines dans machines.csv...")
   println("Fichier sauvegardé avec succès.")
   writer.close()
  } catch {
   case e: Exception => println("Erreur : Échec de l'écriture dans machines.csv." +
     "\nLe fichier peut être verrouillé ou en lecture seule.")
  }
 }


 // Début du programme
val machines = loadcsv("src/machines.csv")
 var boucle = true
 while (boucle) {
var choixMachine = -1

  // Sélection du mode
  println("\n       Nospresso café              ")

  println("Veuiller choisir votre mode : ")
  println("1) Client")
  println("2) Admin")
  println("3) Quitter")
  var choix = readLine("> ").toInt
  while (choix < 1 || choix > 3) choix = readLine("Veuillez selectionner 1,2 ou 3 : \n> ").toInt

  // Mode client
  if (choix == 1) {
   choixMachine = readLine("Veuillez sélectionner une machine (1-5) : \n> ").toInt - 1
   while (choixMachine < 0 || choixMachine > 5) {
    choixMachine = readLine("Veuillez sélectionner un numéro valide : \n> ").toInt - 1
   }
   println("Vous avez sélectionné la Machine " + (choixMachine + 1))
   val machine = machines(choixMachine)
   machines(choixMachine).serveClient(machine.id)
  }

  // Mode Admin
  if (choix == 2) {
   choixMachine = readLine("Veuillez sélectionner une machine (1-5) : \n> ").toInt - 1
   while (choixMachine < 0 || choixMachine > 5) {
    choixMachine = readLine("Veuillez sélectionner un numéro valide : \n> ").toInt - 1
   }
   println("Vous avez sélectionné la Machine " + (choixMachine + 1))
 val machine = machines(choixMachine)
   if (machine.validatePin(machine.id, machine.pincode)) {
  println("1) Réapprovisionnement des stocks")
    println("2) Changement de code Pin")
    var choixAdmin = readLine("> ").toInt
    if (choixAdmin == 1) {
     machines(choixMachine).restockMachine(machine.id)
    }
    if (choixAdmin == 2){
     machines(choixMachine).updatePin(machine.id)
    }
   }
  }

  // Quitter le programme
  if (choix == 3){
   println("Merci d'avoir utilisé Nospresso. À bientôt !")
   savecsv("src/machines.csv", machines)
   boucle = false
  }
 }

 }