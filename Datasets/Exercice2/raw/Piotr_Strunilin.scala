object Main {
  import io.StdIn._
  import util.Random
  def main(args: Array[String]): Unit = {
    // variables below are necessary for loops and inputs
    var sort = ""; var kind = ""; var like = ""; var attempt_machine_pin = ""// descriptions for the item selected and pin attempt for a specific machine
    var leave = false; var cerr = false; var merr = false; var serr = false; var out = false; // for menus, errors and outputs
    var coffee = 0; var sugar = 0; var milk = 0; var size = 0 // choices

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
    // Note that this is done only for prices since you can have more specific quantities for the masses and volumes above.
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

    val nbMachines = 5; var machine_selected = 0; val nb_attempts = 3; var choice = 0; var mode = 0; var admin_mode = 0; var client_mode = 0; // variables used to run the collection of machines
    var quit = false //used to terminate the interaction
    // below are initial values for the collection of machines
    val machinePIN = Array.fill(nbMachines){"434343"} //default passwords
    val coffeeStk = Array.fill(nbMachines){50} //these are grams
    val milkStk = Array.fill(nbMachines){500} //these are in ml
    val sugarStk = Array.fill(nbMachines){30}
    // below takes user input to a message and ensures that its bounded
    def choose(n: Int, m: Int, msg: String): Int = {
      do{
        print(msg)
        choice = readInt()
      }while(choice<n||choice>m)
      choice
    }
    //below does calculations for the main prices
    def calculation(name: String, qty_c: Int, qty_m: Int, price: BigDecimal, difference: BigDecimal,coffeeStocks:Array[Int], milkStocks:Array[Int], machineId: Int): BigDecimal ={
          sort = name
          qty_coffee_sel = qty_c
          coffeeStocks(machineId) -= qty_coffee_sel
          part_sum = price + difference
          qty_milk_sel = qty_m
          milkStocks(machineId) -= qty_milk_sel
          sum += part_sum
      sum
    }
    //below does calculations for supplementary prices
    def calculation_sup(name: String,tag: String ,qty_s: Int, qty_add_m: Int, price_sup_sugar: BigDecimal,price_sup_milk:BigDecimal, doses: Int,sugarStocks:Array[Int], milkStocks:Array[Int], machineId: Int): BigDecimal = {
      kind += name
      like += tag
      qty_sugar_sel += qty_s
      sugarStocks(machineId) -= qty_sugar_sel
      sup_sum += price_sup_sugar
      qty_add_milk_sel += doses * qty_add_m
      milkStocks(machineId) -= qty_add_milk_sel
      sum += doses*price_sup_milk+price_sup_sugar
      sum
    }
    // below checks validity of the attempted pin
    def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
      var attempt = nb_attempts
      var correctness = false
      var pin_attempt = ""
      print("Entrez le code PIN : \n>")
      while (attempt > 0 && pin_attempt != machinePins(machineId)) {
        attempt -= 1
        pin_attempt = readLine()
        if(pin_attempt != machinePins(machineId)){
          print("Code PIN incorrect. "+attempt+" tentatives restantes.")
          if(attempt!=0)print("\n>")
        }
      }
      if(pin_attempt==machinePins(machineId)) correctness = true
      else if(attempt==0 && pin_attempt!=machinePins(machineId)){
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
    def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
      println("Mise à jour du code PIN pour la machine "+(machineId+1)+".")
      do{
        print("Entrez un nouveau code PIN à 6 chiffres \n>")
        machinePins(machineId) = readLine()
      }while(machinePins(machineId).length!=6 && machinePins(machineId).forall(_.isDigit))
      println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")
    }
    // below does the client service
    def serveClient(machineId: Int, coffeeStocks: Array[Int],sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
      client_mode = 0 // resets the client's choice
          while (!(client_mode==1||client_mode==2||client_mode==3) || !leave){ //mode selection
            val rnd = Random.alphanumeric.take(5).mkString("").toUpperCase() // twint code is done by generating 5 alphanumeric chars and then making them into an uppercase sting
            client_mode = choose(1,3,"\nNospresso Cafe\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>")
            if(client_mode==1) {
              coffee = choose(1,3,"\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF "+ price_exp +"\n2) Cappuccino - CHF "+ price_cap +"\n3) Latte - CHF " + price_lat + " (Petit), CHF " + (price_lat + price_inc) + " (Moyen), CHF " + (price_lat + 2*price_inc) + " (Grand)\n>")
              if (coffee == 1) {
                if(coffeeStocks(machineId)>=medium) { // if there is more coffee than what the drink requires, then we make it. If not we throw an error
                  calculation("Expresso",medium,0,price_exp,0,coffeeStocks,milkStocks, machineId)
                }else cerr=true
              }
              else if (coffee == 2) {
                if(milkStk(machineId)>=vol_cap){ //same logic as above with coffee, but now with both milk and coffee
                  if(coffeeStocks(machineId)>=small){
                    calculation("Cappuccino",small,vol_cap,price_cap,0,coffeeStocks,milkStocks, machineId)
                  }else cerr= true
                }else merr = true
              }
              else if (coffee == 3) { //size of latte selection
                size = choose(1,3,"\nVeuillez sélectionner la taille de votre café latte :\n1)  (Petit), CHF " + price_lat + "\n2)  (Moyen), CHF " + (price_lat + price_inc) + "\n3)  (Grand), CHF " + (price_lat + 2*price_inc) + "\n>")
                if (size == 1) {
                  if(milkStocks(machineId)>=vol_lat_p){
                    if(coffeeStocks(machineId)>=small){
                      calculation("Latte (Petit)",small,vol_lat_p,price_lat,0,coffeeStocks,milkStocks, machineId)
                    } else cerr = true
                  } else merr = true
                }
                else if (size == 2) {
                  if(milkStocks(machineId)>=vol_lat_m){
                    if(coffeeStocks(machineId)>=medium){
                      calculation("Latte (Moyen)",medium,vol_lat_m,price_lat,price_inc,coffeeStocks,milkStocks, machineId)
                    } else cerr = true
                  } else merr = true
                }
                else if (size == 3) {
                  if(milkStocks(machineId)>=vol_lat_g){
                    if(coffeeStocks(machineId)>=large){
                      calculation("Latte (Grand)",large,vol_lat_g,price_lat,2*price_inc,coffeeStocks,milkStocks, machineId)
                    } else cerr = true
                  } else merr = true
                }
              }
              // below is selection for sugar
              sugar = choose(1,4,"\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF "+price_sugar+"\n3) Moyen (10g) - CHF "+(2*price_sugar)+"\n4) Beaucoup (15g) - CHF "+(3*price_sugar)+"\n>") //gave a weird double not what expected
              if (sugar == 1 && !cerr && !merr)calculation_sup("Sans sucre","",0,0,0,0,0,sugarStocks,milkStocks, machineId)
              else if (sugar == 2 && !cerr && !merr) {
                if(sugarStocks(machineId)>=delta_s) calculation_sup("Peu (5g)","",delta_s,0,price_sugar,0,0,sugarStocks,milkStocks, machineId)
                else serr = true
              }
              else if (sugar == 3 && !cerr && !merr) {
                if(sugarStocks(machineId)>= 2*delta_s) calculation_sup("Moyen (10g)","",2*delta_s,0,2*price_sugar,0,0,sugarStocks,milkStocks, machineId)
                else serr = true
              }
              else if (sugar == 4 && !cerr && !merr) {
                if(3*delta_s <= sugarStocks(machineId)) calculation_sup("Beaucoup (15g)","",3*delta_s,0,3*price_sugar,0,0,sugarStocks,milkStocks, machineId)
                else serr = true
              }
              // below is milk selection
              if (coffee != 1) {
                milk = choose(1,2,"Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n>")
                if (milk == 1) {
                  sup_milk_doses = choose(0,3,"Combien de doses de lait souhaitez-vous ? (au plus 3)\n>")
                  if (delta_m * sup_milk_doses <= milkStocks(machineId)) calculation_sup("","Oui",0,delta_m,0,price_sup_milk,sup_milk_doses,sugarStocks,milkStocks, machineId)
                  else merr = true
                }
                else if (milk == 2) calculation_sup("","Non",0,0,0,0,0,sugarStocks,milkStocks, machineId)
              }
              // below is correction for the amount if there is not enough of milk or sugar
              if(serr||merr){
                coffeeStocks(machineId)+=qty_coffee_sel
                milkStocks(machineId)+=qty_milk_sel
              }
              if(serr) milkStocks(machineId)+=qty_add_milk_sel
              if(merr) sugarStocks(machineId)+=qty_sugar_sel

              println("Boisson sélectionnée : " + sort +
                "\nNiveau de sucre : " + kind)
              if (coffee != 1) println("Lait supplémentaire : " + like)
              // below are error messages
              if (cerr) error_print(1)
              else if (serr) error_print(2)
              else if (merr) error_print(3)
              // below is the payment process
              if (!cerr && !serr && !merr) {
                out = true
                print("Prix total : CHF " + part_sum)
                if (sugar != 1) print(" + CHF " + sup_sum)
                if (milk == 1) print(" + CHF " + sup_milk_doses * price_sup_milk)
                print(" = CHF " + sum)
                print("\n\nVeuillez payer en utilisant Twint." +
                  "\nVotre code de paiement est : " + rnd +
                  "\n(En attente de paiement...)")
                Thread.sleep(3000)
                print("\n\nPaiement confirmé." +
                  "\nPréparation de votre boisson..." +
                  "\nVotre ")
                if(coffee!=3) print(sort)
                else print("Latte")
                println(" est prêt ! Bonne dégustation !")
                leave = true
              }
              // below reset all the choices at the end of the order
              sum = 0.0; sup_sum=0.0;qty_milk_sel=0; qty_add_milk_sel=0; qty_coffee_sel = 0; qty_sugar_sel = 0
              coffee = 0; size = 0; sugar = 0; milk = 0; like = ""; kind = ""; sort = ""; cerr=false; serr=false; merr=false
            }// below is the specific machine admin mode
            else if(client_mode==2) {
              print("\nEntrez le code PIN : ")
              attempt_machine_pin = readLine()
              if (attempt_machine_pin == machinePIN(machineId)) {
                println("Accès autorisé.")
                attempt_machine_pin = "" // so that the correct attempt is not stored
                println("\nStocks :")
                println("Poudre de café : " + coffeeStocks(machineId) + "g\nSucre : " + sugarStocks(machineId) + "g\nLait : 0." + milkStocks(machineId) +"L")

                println("\nRéapprovisionnement des stocks...")
                println("Ajout :")
                print("Poudre de café : ")
                coffeeStocks(machineId) += readInt()
                if (coffeeStocks(machineId) < 0) coffeeStocks(machineId) = 0 // you can't have negative mass in the machine...
                print("Sucre: ")
                sugarStocks(machineId) += readInt()
                if (sugarStocks(machineId) < 0) sugarStocks(machineId) = 0
                print("Lait: ")
                milkStocks(machineId) += readInt()
                if (milkStocks(machineId) < 0) milkStocks(machineId) = 0
                println("Niveaux de stock mise à jour.")
                println("Retour au menu principal...")
              }
            }
            else if(client_mode==3) leave = true
          }
      out
    }
    // below is restocking method
    def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
      println("Niveaux de stock actuels :\nCafé en poudre : " + coffeeStocks(machineId) + "g\nSucre : " +sugarStocks(machineId) + "g\nLait : 0." + milkStocks(machineId) + "L\n")
      print("Entrez les quantités à ajouter : \nPoudre de café > ") // this assumes that the stocks are in grams
      coffeeStocks(machineId) += readInt()
      print("Sucre > ")
      sugarStocks(machineId) += readInt()
      print("Lait > ")
      milkStocks(machineId) += readInt()
      println("Les stocks ont été mis à jour avec succès.\nRetour au menu principal...")
    }
    // below is the mode selection for all the machines
    while (!(mode==1||mode==2||mode==3) || !quit) {
      print("\nVeuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n>")
      mode = readInt()
      if (mode == 1) { // client
        machine_selected = choose(1, nbMachines, "Machine sélectionnée (1-" + nbMachines + ") \n>") - 1
        serveClient(machine_selected,coffeeStk,sugarStk,milkStk)
      }
      else if (mode == 2) { // admin
        machine_selected = choose(1, nbMachines, "Machine sélectionnée (1-" + nbMachines + ") \n>") - 1
        var trigger = validatePin(machine_selected,machinePIN)
        if(trigger){
          print("Accès est accordé à une machine " + (machine_selected + 1) + ".")
          while (!(admin_mode == 1 || admin_mode == 2)) { // Admin choices
            print("\n1) Réapprovisionnement en ingrédients  \n2) Mise à jour du code PIN \n>")
            admin_mode = readInt()
            if (admin_mode == 1) {
              restockMachine(machine_selected, coffeeStk, sugarStk, milkStk)
            }
            else if(admin_mode ==2) updatePin(machine_selected, machinePIN)
          }
        }
        trigger = false; admin_mode = 0 // rests the triggers
      }
      else if(mode == 3) quit = true
    }
  }
}