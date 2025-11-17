import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine
import java.io.{File, PrintWriter}
import scala.io.StdIn.readInt

class Machine(val id: Int, var pincode: String, var milk: Double, var sugar: Double, var coffee: Double) {
    def addIngredient(ingredient: String, amount: Double): Unit = {
        if (ingredient == "milk") {
            milk += amount
        } else if (ingredient == "coffee") {
            coffee += amount
        } else if (ingredient == "sugar") {
            sugar += amount
        } else {
            println("Erreur ! Ingrédient introuvable !")
        }
    }

    def removeIngredient(ingredient: String, amount: Double): Boolean = {
        if (ingredient == "milk") {
            if (milk - amount > 0) {
                milk -= amount
                return true
            } else {
                println("Pas assez de " + ingredient)
                return false
            }
        } else if (ingredient == "coffee") {
            if (coffee - amount > 0) {
                coffee -= amount
                return true
            } else {
                println("Pas assez de " + ingredient)
                return false
            }
        } else if (ingredient == "sugar") {
            if (sugar - amount > 0) {
                sugar -= amount
                return true
            } else {
                println("Pas assez de " + ingredient)
                return false
            }
        } else {
            println("Erreur ! Ingrédient introuvable !")
            return false
        }
    }
}


object Main {
    var nbMachines = 0
    var machines: ArrayBuffer[Machine] = ArrayBuffer()
    def loadcsv(filename: String): ArrayBuffer[Machine] = {
        val machines = ArrayBuffer[Machine]()
        try {
            println("Lecture du fichier : " + filename)
            val file = Source.fromFile(filename)
            val lines = file.getLines()
            lines.next()
            for (line <- lines) {
                val parts = line.split(",")
                if (parts.length == 4) {
                    val id = machines.size + 1
                    val pin = parts(0).trim
                    val milk = parts(1).trim.toDouble
                    val sugar = parts(2).trim.toDouble
                    val coffee = parts(3).trim.toDouble
                    
                    machines += new Machine(id, pin, milk, sugar, coffee)
                    println("Machine chargée :")
                    println("  ID : " + id)
                    println("  Code PIN : " + pin)
                    println("  Lait : " + milk/1000 + "L")
                    println("  Sucre : " + sugar + "g")
                    println("  Café : " + coffee + "g")
                } else {
                    println("Ligne invalide ignorée : " + line)
                }
            }
            file.close()
            println("Nombre total de machines chargées : " + machines.size)
        } catch {
            case e: Exception =>
                println("Erreur lors de la lecture du fichier : " + e.getMessage)
                System.exit(1)
        }
        machines
    }

    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
        try {
            val writer = new PrintWriter(new File(filename))
            writer.println("PINCODE,MILK,SUGAR,COFFEE")
            for (machine <- machines) {
                writer.println(machine.pincode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
            }
            writer.close()
            println("Machines sauvegardées dans le fichier : " + filename)
        } catch {
            case e: Exception =>
                println("Erreur lors de l'écriture dans le fichier : " + e.getMessage)
                System.exit(1)
        }
    }

    def main(args: Array[String]): Unit = {
        var mode = 0
        var repeat = false

        machines = loadcsv("machines.csv")
        nbMachines = machines.length

        while(!repeat) {
            println("Choisir une machine (1-5):")
            var machineId = readLine().toInt
            if (machineId >= 1 && machineId < nbMachines + 1) {
                machineId -= 1
                println("1) Client\n2) Admin\n3) Sortie")
                mode = readLine().toInt
                if (mode == 1) {
                    serveClient(machineId, machines)
                }
                if (mode == 2) {
                    if (validatePin(machineId, machines)) {
                        println("1) Stockage de la machine\n2) Mise a jour du code PIN")
                        val adminChoice = readLine().toInt
                        if (adminChoice == 1) {
                            restockMachine(machineId, machines)
                        }
                        else if (adminChoice == 2) {
                            updatePin(machineId, machines)
                        }
                        else {
                            println("Option invalide. Retour au menu principal")
                        }
                    }
                }
                if (mode == 3) {
                    println("Merci a bientot!")
                    savecsv("machines.csv", machines)
                    repeat = true
                }
            }
        }
    }


    def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
        val prixExpresso    = 2.0
        val prixCapuccino   = 2.5
        val prixPetitLatte  = 2.7
        val prixMoyenLatte  = 3.2
        val prixGrandLatte  = 3.7

        val supplementPetitLait  = 0.05
        val supplementMoyenLait  = 0.10
        val supplementGrandLait  = 0.15

        val supplementPetitSucre = 0.10
        val supplementMoyenSucre = 0.20
        val supplementGrandSucre = 0.30

        val usageCafeEspresso    = 8
        val usageMilkEspresso    = 0

        val usageCafeCappuccino  = 8
        val usageMilkCappuccino  = 100

        val usageCafeLattePetit  = 6
        val usageCafeLatteMoyen  = 8
        val usageCafeLatteGrand  = 12

        val usageMilkLattePetit  = 120
        val usageMilkLatteMoyen  = 150
        val usageMilkLatteGrand  = 200

        val plusPetitLait = 50
        val plusMoyenLait = 100
        val plusGrandLait = 150

        val usageSugarSmall  = 5
        val usageSugarMedium = 10
        val usageSugarLarge  = 15

        var totalPrice      = 0.0
        var coffeeUsed      = 0
        var milkUsed        = 0
        var extraMilkUsed   = 0
        var sugarUsed       = 0

        var priceExtraMilk  = 0.0
        var priceExtraSugar = 0.0

        val alphaNumeric  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        val code_twint    = (1 to 5).map(_ => alphaNumeric((math.random * alphaNumeric.length).toInt)).mkString

        println("Quelle boisson voulez-vous? \n1) Espresso \n2) Cappuccino \n3) Latte")
        val boisson_choisi = readLine().toInt

            if (boisson_choisi == 1) {
            totalPrice = prixExpresso
            coffeeUsed = usageCafeEspresso
            milkUsed   = usageMilkEspresso
        }

        if (boisson_choisi == 2) {
            totalPrice = prixCapuccino
            coffeeUsed = usageCafeCappuccino
            milkUsed   = usageMilkCappuccino

            println("Souhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Cappuccino et Latte)\n(1) Oui\n(2) Non")
            val extraMilkChoice = readInt()
            if (extraMilkChoice == 1) {
                println("Combien de dose voulez-vous ? (1 = petit, 2 = moyen, 3 = grand)")
                val dose = readInt()
                    if (dose == 1) {
                    extraMilkUsed = plusPetitLait
                    priceExtraMilk = supplementPetitLait
                    } else if (dose == 2) {
                    extraMilkUsed = plusMoyenLait
                    priceExtraMilk = supplementMoyenLait
                    } else if (dose == 3) {
                    extraMilkUsed = plusGrandLait
                    priceExtraMilk = supplementGrandLait
                    } else {
                    extraMilkUsed = 0
                    priceExtraMilk = 0.0
                }
            }
        }

        if (boisson_choisi == 3) {
            println("De quelle taille souhaitez-vous votre latte?\n" +
                "(1) Petit\n(2) Moyen\n(3) Grand")
            val tailleLatte = readInt()

            if (tailleLatte == 1) {
                totalPrice = prixPetitLatte
                coffeeUsed = usageCafeLattePetit
                milkUsed   = usageMilkLattePetit
            } else if (tailleLatte == 2) {
                totalPrice = prixMoyenLatte
                coffeeUsed = usageCafeLatteMoyen
                milkUsed   = usageMilkLatteMoyen
            } else if (tailleLatte == 3) {
                totalPrice = prixGrandLatte
                coffeeUsed = usageCafeLatteGrand
                milkUsed   = usageMilkLatteGrand
            }

            println("Voulez-vous ajouter encore plus de lait?\n" +
                "(0) Non\n(1) Petit supplément\n(2) Moyen supplément\n(3) Grand supplément")
            val plusLaitChoice = readLine().toInt
            if (plusLaitChoice == 1) {
                extraMilkUsed = plusPetitLait
                priceExtraMilk = supplementPetitLait
            } else if (plusLaitChoice == 2) {
                extraMilkUsed = plusMoyenLait
                priceExtraMilk = supplementMoyenLait
            } else if (plusLaitChoice == 3) {
                extraMilkUsed = plusGrandLait
                priceExtraMilk = supplementGrandLait
            } else {
                extraMilkUsed = 0
                priceExtraMilk = 0.0
            }
        }

        println("Voulez-vous ajouter du sucre?\n" +
        "(0) Non\n(1) Petit sucre\n(2) Moyen sucre\n(3) Grand sucre")
        val sucreChoice = readInt()
        if (sucreChoice == 1) {
            sugarUsed = usageSugarSmall
            priceExtraSugar = supplementPetitSucre
        } else if (sucreChoice == 2) {
            sugarUsed = usageSugarMedium
            priceExtraSugar = supplementMoyenSucre
        } else if (sucreChoice == 3) {
            sugarUsed = usageSugarLarge
            priceExtraSugar = supplementGrandSucre
        } else {
            sugarUsed = 0
            priceExtraSugar = 0.0
        }

        totalPrice += priceExtraMilk + priceExtraSugar

        val totalMilkNeeded = milkUsed + extraMilkUsed
        if (
        machines(machineId).coffee >= coffeeUsed &&
            machines(machineId).sugar >= sugarUsed &&
            machines(machineId).milk >= totalMilkNeeded
        ){ 
            println("Le prix total à payer est : " + totalPrice + " CHF")
            println("Paiement par Twint\nVotre code pour le paiement est : " + code_twint)
            Thread.sleep(1000)
            Thread.sleep(1000)
            println("Le paiement a bien été accepté.\nPréparation de votre boisson...\n[...]\nVotre Café est prêt ! Bonne dégustation !")

            machines(machineId).removeIngredient("coffee",coffeeUsed)
            machines(machineId).removeIngredient("milk",totalMilkNeeded)
            machines(machineId).removeIngredient("sugar",sugarUsed)
            return true
        } else {
            println("Désolé, la machine n'a pas assez de stock pour cette commande.")
            return false
        }
    }


    def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean =  {
        println("Entrez un PIN pour la machine: " + (machineId+1) + ":")
        var attempts = 3
        while (attempts > 0) {
            val inputPin = readLine()
            if (inputPin == machines(machineId).pincode) {
                println("Code PIN bon.")
                return true
            }
            else {
                attempts -= 1
                println("PIN incorrect. " + attempts + " essaies restants")
            }
        }
        println("Vous avez depasser le nombre d'essaies maximum")
        false
    }


    def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
        println("Mise a jour du code PIN de la machine: " + (machineId + 1))
        def isValidPin(pin: String): Boolean = {
            if (pin.length != 6) {
                return false
            }   
                
            for (char <- pin) {
                if (!char.isDigit){
                    return false
                }
            }
            true
        }

        var codeValide = false
            while (!codeValide) {
            println("Entrez un code PIN de 6 chiffres")
            val pinAsString = readLine()

            if (isValidPin(pinAsString)) {
                machines(machineId).pincode = pinAsString
                println("Code PIN mis a jour avec succes.")
                codeValide = true
            } else {
                println("Code PIN invalide. Assurez-vous qu'il contient exactement 6 chiffres.")
            }
        }
    }

    def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
        println(" voici les stock de cafe : " + machines(machineId).coffee)
        println(" voici les stock de sucre : " + machines(machineId).sugar)
        println(" voici les stock de lait : " + (machines(machineId).milk/1000))

        println("combien de g de sucre souhaite tu ajouter au stock")
        machines(machineId).addIngredient("sugar", readInt())

        println("combien de g de cafe souhaite tu ajouter au stock")
        machines(machineId).addIngredient("coffee", readInt())

        println("combien de lait en L souhaite tu ajouter au stock")
        val lait_ajoute = readLine().toDouble
        machines(machineId).addIngredient("milk", (lait_ajoute * 1000.0).toInt + machines(machineId).milk)
    }
}