
object Main {
  import io.StdIn._
  import util.Random
  import scala.io.Source
  import scala.collection.mutable.ArrayBuffer
  import java.io.{FileWriter, PrintWriter}

  def main(args: Array[String]): Unit = {
    val nb_attempts = 3;var choice = 0;var mode = 0;var admin_mode = 0;var client_mode = 0; // variables used to run the collection of machines
    var quit = false //used to terminate the interaction
    var machine_selected = 0
    def choose(n: Int, m: Int, msg: String): Int = { // has to be declared and defined outside the class since its used all across the program.
      do{
        print(msg)
        choice = readInt()
      }while(choice<n||choice>m)
      choice
    }
    class Machine(val id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
        // variables below are necessary for loops and inputs
        // NOTE: all of these attribute could be made private as there is no need for them to be accessed by anyone outside the class.
        var sort = "";var kind = "";var like = "";var attempt_machine_pin = "" // descriptions for the item selected and pin attempt for a specific machine
        var done = false;var leave = false;var cerr = false;var merr = false;var serr = false;var out = false; // for menus, errors and outputs
        var choice_coffee = 0;var choice_sugar = 0; var choice_milk = 0;var size = 0 // choices
        var qty_coffee_sel = 0 // qty of coffee selected by user
        var qty_milk_sel = 0 //...milk...
        var qty_add_milk_sel = 0 //... additional milk...
        var qty_sugar_sel = 0 // ... sugar...
        var sup_milk_doses = 0 // number of doses of milk
        val delta_s = 5 // how much sugar is given
        val delta_m = 50 // how much extra milk is given per dose
        val small = 6 // small cup of coffee in g
        val medium = 8 // medium...
        val large = 12 // large...
        val vol_cap = 100 // volume of milk for cappuccino
        val vol_lat_p = 120 // ... for a small latte
        val vol_lat_m = 150 // ... for a medium ...
        val vol_lat_g = 200 // ... for a large...
        // variables below are specifically for pricing. Rounded up to 2 decimal places.
        // Note that this is done only for prices since you could have more specific quantities for the masses and volumes above.
        // Note I have to use BigDecimal otherwise the computer fails to give accurate answers
        var sum = BigDecimal(0.0).setScale(2, BigDecimal.RoundingMode.HALF_UP) //cumulative cost of the order
        var part_sum = BigDecimal(0.0).setScale(2, BigDecimal.RoundingMode.HALF_UP) // cost of an individual selection
        var sup_sum = BigDecimal(0.0).setScale(2, BigDecimal.RoundingMode.HALF_UP) // cost of supplements
        val price_exp = BigDecimal(2.00).setScale(2, BigDecimal.RoundingMode.HALF_UP) // cost of expresso in CHF
        val price_cap = BigDecimal(2.50).setScale(2, BigDecimal.RoundingMode.HALF_UP) // ... of cappuccino
        val price_lat = BigDecimal(2.70).setScale(2, BigDecimal.RoundingMode.HALF_UP) // ... base cost of latte
        val price_inc = BigDecimal(0.5).setScale(2, BigDecimal.RoundingMode.HALF_UP) //... increase in cost of latte per increase in size of latte
        val price_sugar = BigDecimal(0.1).setScale(2, BigDecimal.RoundingMode.HALF_UP) // cost of a single sugar dose
        val price_sup_milk = BigDecimal(0.05).setScale(2, BigDecimal.RoundingMode.HALF_UP) // cost of a supplementary milk dose

      //below does calculations for the main prices
      def calculation(name: String, qty_c: Int, qty_m: Int, price: BigDecimal, difference: BigDecimal): BigDecimal ={
        sort = name
        qty_coffee_sel = qty_c
        coffee -= qty_coffee_sel
        part_sum = price + difference
        qty_milk_sel = qty_m
        milk -= qty_milk_sel
        sum += part_sum
        sum
      }
      //below does calculations for supplementary prices
      def calculation_sup(name: String,tag: String ,qty_s: Int, qty_add_m: Int, price_sup_sugar: BigDecimal,price_sup_milk:BigDecimal, doses: Int): BigDecimal = {
        kind += name
        like += tag
        qty_sugar_sel += qty_s
        sugar -= qty_sugar_sel
        sup_sum += price_sup_sugar
        qty_add_milk_sel += doses * qty_add_m
        milk -= qty_add_milk_sel
        sum += doses*price_sup_milk+price_sup_sugar
        sum
      }
      // below checks validity of the attempted pin
      def validatePin(): Boolean = {
        var attempt = nb_attempts
        var correctness = false
        var pin_attempt = ""
        print("Entrez le code PIN : \n>")
        while (attempt > 0 && pin_attempt != pincode) {
          attempt -= 1
          pin_attempt = readLine()
          if(pin_attempt != pincode){
            print("Code PIN incorrect. "+attempt+" tentatives restantes.")
            if(attempt!=0)print("\n>")
          }
        }
        if(pin_attempt==pincode) correctness = true
        else if(attempt==0 && pin_attempt!=pincode){
          print("\n\nTrop de tentatives échouées. Fin du programme.")
          quit = true
        }
        correctness
      }
      // below prints out an error
      def error_print(err: Int):Unit = {
        print("\nErreur : Quantité de")
        if(err==1) print(" poudre de café ")
        else if(err==2) print(" sucre ")
        else if(err==3) print(" lait ")
        if(err==1||err==2) print("insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        else print("insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
      }
      // below updates pin for a specific machine
      def updatePin(): Unit = {
        println("Mise à jour du code PIN pour la machine "+id+".")
        do{
          print("Entrez un nouveau code PIN à 6 chiffres \n>")
          pincode = readLine()
        }while(pincode.length!=6 || !pincode.forall(Character.isDigit))
        println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")
      }
      def addIngredient(ingredient: String, amount: Int): Unit = { // Uses user input to add an ingredient of a certain quantity (same with removeIngredient method) but it only takes in english...
        if(ingredient=="poudre de café" && amount>=0) coffee += amount
        else if(ingredient=="lait" && amount>=0) milk += amount
        else if(ingredient=="sucre" && amount>=0) sugar += amount
        else if (ingredient!="poudre de café"||ingredient!="lait"||ingredient!="sucre") println("Ingrédient non valable: " + ingredient)
        if(amount<0) println("Une quantité inacceptable a été fournie.")
      }
      def removeIngredient(ingredient: String, amount: Int): Boolean = {
        var out = true
        if(ingredient=="poudre de café" && coffee>=amount && amount>=0) coffee -= amount
        else if(ingredient=="lait" && milk>=amount && amount>=0) milk -= amount
        else if(ingredient=="sucre" && sugar>=amount && amount>=0) sugar -= amount
        else if (ingredient!="poudre de café"||ingredient!="lait"||ingredient!="sucre"){
          println("Ingrédient non valable: " + ingredient)
          out = false
        }
        if(amount<0 || amount>coffee || amount>sugar || amount>milk) println("Une quantité inacceptable a été fournie.")
        out
      }
      def restockMachine(): Unit = {
        println("Niveaux de stock actuels :\nCafé en poudre : " + coffee + "g\nSucre : " +sugar + "g\nLait : " + (milk/1000) + "L\n")
        do{
          choose(1, 2, "1) Ajouter\n2) Enlever \n>")
          if (choice == 1) {
            print("Quel ingrédient souhaitez-vous ajouter?\n>")
            val ing = readLine()
            print("Quelle quantité de " + ing + " voulez-vous ajouter? \n>")
            val amt = readInt()
            addIngredient(ing, amt)
          }
          else if (choice == 2) {
            print("Quel ingrédient souhaitez-vous supprimer?\n>")
            val ing = readLine()
            print("Quelle quantité de " + ing + " voulez-vous supprimer? \n>")
            val amt = readInt()

            removeIngredient(ing, amt)
          }
          choose(1,2,"Souhaitez-vous continuer à modifier le stock? \n1) Oui \n2) Non \n>")
          if(choice==2) done = true
        }while(!done)
        println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...")
      }
      def serveClient(): Boolean = {
        client_mode = 0 // resets the client's choice
        while (!(client_mode==1||client_mode==2||client_mode==3) || !leave){ //mode selection
          val rnd = Random.alphanumeric.take(5).mkString("").toUpperCase() // twint code is done by generating 5 alphanumeric chars and then making them into an uppercase sting
          client_mode = choose(1,3,"\nNospresso Cafe\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>")
          if(client_mode==1) {
            choice_coffee = choose(1,3,"\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF "+ price_exp +"\n2) Cappuccino - CHF "+ price_cap +"\n3) Latte - CHF " + price_lat + " (Petit), CHF " + (price_lat + price_inc) + " (Moyen), CHF " + (price_lat + 2*price_inc) + " (Grand)\n>")
            if (choice_coffee == 1) {
              if(coffee>=medium) { // if there is more coffee than what the drink requires, then we make it. If not we throw an error
                calculation("Expresso",medium,0,price_exp,0)
              }else cerr=true
            }
            else if (choice_coffee == 2) {
              if(milk>=vol_cap){ //same logic as above with coffee, but now with both milk and coffee
                if(coffee>=small){
                  calculation("Cappuccino",small,vol_cap,price_cap,0)
                }else cerr= true
              }else merr = true
            }
            else if (choice_coffee == 3) { //size of latte selection
              size = choose(1,3,"\nVeuillez sélectionner la taille de votre café latte :\n1)  (Petit), CHF " + price_lat + "\n2)  (Moyen), CHF " + (price_lat + price_inc) + "\n3)  (Grand), CHF " + (price_lat + 2*price_inc) + "\n>")
              if (size == 1) {
                if(milk>=vol_lat_p){
                  if(coffee>=small){
                    calculation("Latte (Petit)",small,vol_lat_p,price_lat,0)
                  } else cerr = true
                } else merr = true
              }
              else if (size == 2) {
                if(milk>=vol_lat_m){
                  if(coffee>=medium){
                    calculation("Latte (Moyen)",medium,vol_lat_m,price_lat,price_inc)
                  } else cerr = true
                } else merr = true
              }
              else if (size == 3) {
                if(milk>=vol_lat_g){
                  if(coffee>=large){
                    calculation("Latte (Grand)",large,vol_lat_g,price_lat,2*price_inc)
                  } else cerr = true
                } else merr = true
              }
            }
            // below is selection for sugar
            choice_sugar = choose(1,4,"\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF "+price_sugar+"\n3) Moyen (10g) - CHF "+(2*price_sugar)+"\n4) Beaucoup (15g) - CHF "+(3*price_sugar)+"\n>") //gave a weird double not what expected
            if (choice_sugar == 1 && !cerr && !merr) calculation_sup("Sans sucre","",0,0,0,0,0)
            else if (choice_sugar == 2 && !cerr && !merr) {
              if(sugar>=delta_s) calculation_sup("Peu (5g)","",delta_s,0,price_sugar,0,0)
              else serr = true
            }
            else if (choice_sugar == 3 && !cerr && !merr) {
              if(sugar>= 2*delta_s) calculation_sup("Moyen (10g)","",2*delta_s,0,2*price_sugar,0,0)
              else serr = true
            }
            else if (choice_sugar == 4 && !cerr && !merr) {
              if(3*delta_s <= sugar) calculation_sup("Beaucoup (15g)","",3*delta_s,0,3*price_sugar,0,0)
              else serr = true
            }
            // below is milk selection
            if (choice_coffee != 1) {
              choice_milk = choose(1,2,"Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n>")
              if (choice_milk == 1) {
                sup_milk_doses = choose(0,3,"Combien de doses de lait souhaitez-vous ? (au plus 3)\n>")
                if (delta_m * sup_milk_doses <= milk) calculation_sup("","Oui",0,delta_m,0,price_sup_milk,sup_milk_doses)
                else merr = true
              }
              else if (choice_milk == 2) calculation_sup("","Non",0,0,0,0,0)
            }
            // below is correction for the amount if there is not enough of milk or sugar
            if(serr||merr){
              coffee+=qty_coffee_sel
              milk+=qty_milk_sel
            }
            if(serr) milk+=qty_add_milk_sel
            if(merr) sugar+=qty_sugar_sel

            println("Boisson sélectionnée : " + sort +
              "\nNiveau de sucre : " + kind)
            if (choice_coffee != 1) println("Lait supplémentaire : " +like)
            // below are error messages
            if (cerr) error_print(1)
            else if (serr) error_print(2)
            else if (merr) error_print(3)
            // below is the payment process
            if (!cerr && !serr && !merr) {
              out = true
              print("Prix total : CHF " + part_sum)
              if (choice_sugar != 1) print(" + CHF " + sup_sum)
              if (choice_milk == 1) print(" + CHF " + sup_milk_doses * price_sup_milk)
              print(" = CHF " + sum)
              print("\n\nVeuillez payer en utilisant Twint." +
                "\nVotre code de paiement est : " + rnd +
                "\n(En attente de paiement...)")
              Thread.sleep(3000)
              print("\n\nPaiement confirmé." +
                "\nPréparation de votre boisson..." +
                "\nVotre ")
              if(choice_coffee!=3) print(sort)
              else print("Latte")
              println(" est prêt ! Bonne dégustation !")
              leave = true
            }
            // below reset all the choices at the end of the order
            sum = 0.0; sup_sum=0.0;qty_milk_sel=0; qty_add_milk_sel=0; qty_coffee_sel = 0; qty_sugar_sel = 0
            choice_coffee = 0; size = 0; choice_sugar = 0; choice_milk = 0; like = ""; kind = ""; sort = ""; cerr=false; serr=false; merr=false
          }// below is the specific machine admin mode
          else if(client_mode==2) {
            print("\nEntrez le code PIN : ")
            attempt_machine_pin = readLine()
            if (attempt_machine_pin == pincode) {
              println("Accès autorisé.")
              attempt_machine_pin = "" // so that the correct attempt is not stored
              println("\nStocks :")
              println("Poudre de café : " + coffee + "g\nSucre : " + sugar + "g\nLait : " + (milk/1000) +"L")

              println("\nRéapprovisionnement des stocks...")
              println("Ajout :")
              print("Poudre de café : ")
              coffee += readInt()
              if (coffee < 0) coffee = 0 // you can't have negative mass in the machine...
              print("Sucre: ")
              sugar += readInt()
              if (sugar < 0) sugar = 0
              print("Lait: ")
              milk += readInt()
              if (milk < 0) milk = 0
              println("Niveaux de stock mise à jour.")
              println("Retour au menu principal...")
            }
          }
          else if(client_mode==3) leave = true
        }
        out
      }

    }
    def loadcsv(filename: String): ArrayBuffer[Machine] = {
      val machines = ArrayBuffer[Machine]()
      try {
        val lines = Source.fromFile(filename).getLines().drop(1).toList //that way i get the lines to have indexes, it also drops the header.
        for (line <- lines) {
          val cols = line.split(",")
          if (cols.length == 4) {
            machines += new Machine(lines.indexOf(line), cols(0), cols(1).toInt, cols(2).toInt, cols(3).toInt) // adds each data point into a machine i.e. it defines the machines array.
          }
        }
      } catch { // in case of exception we produce error and terminate the program.
        case _: Exception => println("Erreur : Fichier introuvable ou erreur de lecture." +
          "\nErreur: Échec du chargement ou de l'enregistrement des machines." +
          "\nFermeture du programme.")
          System.exit(1)
      }
      machines
    }
    def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = { // this saves the current version of the csv file by starting at the first line
      var noException = true
      try {
        val writer = new PrintWriter(new FileWriter(filename, false)) // this clears and prepares the file to be edited again
        writer.println("PINCODE,MILK,SUGAR,COFFEE") // we add the header, it doesn't really matter that it's a string since we remove it when using the file later.
        for (machine <- machines) { // this converts all the data to string and adds it to the file line by line.
          writer.println(machine.pincode+","+machine.milk.toString+","+machine.sugar.toString+","+machine.coffee.toString)
        }
        writer.close()
        println("\nSauvegarde de "+machines.size+" dans machines.csv...")
      } catch {
        case _: Exception => //error generation if an exception is triggered, then terminates the program.
          noException = false
          println("Erreur : Échec de l'écriture dans machines.csv." +
            "\nErreur: Échec du chargement ou de l'enregistrement des machines." +
            "\nFermeture du programme.")
          System.exit(1)
      }
      if(noException)println("Fichier sauvegardé avec succès.")
    }
    val filename = "machines.csv"
    println("Chargement des machines depuis machines.csv...")
    val machines = loadcsv(filename)
    def affiche(x: Int): Unit = { // This function prints the details demanded in the question.
      print("Machine "+(x+1)+" chargée :\n ID: "+(x+1)+"\n Code PIN: "+machines(x).pincode+"\n Lait: "+machines(x).milk.toDouble/1000+"L\n Sucre: "+machines(x).sugar +
        "g\nCafé: " + machines(x).coffee + "g\n")
    }
    val nbMachines = machines.size

    while (!(mode==1||mode==2||mode==3) || !quit) {
      for(i<- machines.indices){
        affiche(machines(i).id)
      }
      println(machines.size + " machine(s) chargée(s) avec succès.")
      print("\nNospresso Cafe \nVeuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n>")
      mode = readInt()
      if (mode == 1) { // client
       machine_selected = choose(1, nbMachines, "Machine sélectionnée (1-" + nbMachines + ") \n>")
       machines(machine_selected-1).serveClient()
        savecsv("machines.csv", machines)
      }
      else if (mode == 2) { // admin
        machine_selected = choose(1, nbMachines, "Machine sélectionnée (1-" + nbMachines + ") \n>") - 1
        var trigger = machines(machine_selected).validatePin()
        if(trigger){
          print("Accès est accordé à une machine " + (machine_selected + 1) + ".")
          while (!(admin_mode == 1 || admin_mode == 2)) { // Admin choices
            print("\n1) Réapprovisionnement en ingrédients  \n2) Mise à jour du code PIN \n>")
            admin_mode = readInt()
            if (admin_mode == 1) {
              machines(machine_selected).restockMachine()
            }
            else if(admin_mode ==2) machines(machine_selected).updatePin()
          }
        }
        savecsv("machines.csv",machines)
        trigger = false; admin_mode = 0 // rests the triggers
      }
      else if(mode == 3) {
        savecsv("machines.csv",machines)
        quit = true
      }
    }
  }
}