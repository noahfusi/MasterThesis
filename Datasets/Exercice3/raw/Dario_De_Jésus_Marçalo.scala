import Main.nbmachine

import scala.io.StdIn._
import scala.util.Random
import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
object Main {

  var boucle = 0
  var mode = 0
  var nbmachine = 0
  var mdp = ""
  var ajoutcafe = 0
  var ajoutsucre = 0
  var ajoutlait = 0
  val prixE = 2.00
  val prixC = 2.50
  val prixLP = 2.70
  val prixLM = 3.20
  val prixLG = 3.70
  val sucreP = 0.10
  val sucreM = 0.20
  val sucreB = 0.30
  val prixLait = 0.05
  var prixfinal = 0.0
  var boisson = 0
  var sucre = 0
  var lait = 0
  var dose = 0
  var latte = 0
  var machines = new ArrayBuffer[Machine]

  class Machine(id: Int = 0, pincode: String = "", milk: Int = 0, sugar: Int = 0, coffee: Int = 0) {

    var nbmachine = id
    var code = pincode
    var milkStocks = milk
    var sugarStocks = sugar
    var coffeeStocks = coffee


    def addIngredient(amount: Int, ingredient: String): Unit = {
      var ingredient = ""
      var amount = 0
      do {
        ingredient = readLine("Quel ingrédient ?")
      } while (!(ingredient == "sucre" || ingredient == "lait" || ingredient == "cafe"))
      if (ingredient == "lait") {
        do {
          amount = readLine("Combien de millilitre?").toInt
        } while (amount < 0)
        machines(nbmachine - 1).milkStocks += amount
      }
      else if (ingredient == "cafe") {
        do {
          amount = readLine("Combien de grammes ?").toInt
        } while (amount < 0)
        machines(nbmachine - 1).coffeeStocks += amount
      }
      else if (ingredient == "sucre") {
        do {
          amount = readLine("Combien de grammes ?").toInt
        } while (amount < 0)
        machines(nbmachine - 1).sugarStocks += amount
      }
    }

    def removeIngredient(amount: Int, ingredient: String): Boolean = {
      var ingredient = ""
      var amount = 0
      if ((boisson == 1) && (sucre == 1) && (machines(nbmachine - 1).coffeeStocks >= 8)) {
        machines(nbmachine - 1).coffeeStocks -= 8
        return true
      }
      if ((boisson == 1) && (sucre == 2) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 5)) {
        machines(nbmachine - 1).coffeeStocks -= 8
        machines(nbmachine - 1).sugarStocks -= 5
        return true
      }
      if ((boisson == 1) && (sucre == 3) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 10)) {
        machines(nbmachine - 1).coffeeStocks -= 8
        machines(nbmachine - 1).sugarStocks -= 10
        return true
      }
      if ((boisson == 1) && (sucre == 4) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 15)) {
        machines(nbmachine - 1).coffeeStocks -= 8
        machines(nbmachine - 1).sugarStocks -= 15
        return true
      }
      if ((boisson == 1) && (sucre == 1) && (machines(nbmachine - 1).coffeeStocks < 8)) {
        return false
      }
      if ((boisson == 1) && (sucre == 2) && (machines(nbmachine - 1).coffeeStocks < 8)) {
        return false
      }
      if ((boisson == 1) && (sucre == 3) && (machines(nbmachine - 1).coffeeStocks < 8)) {
        return false
      }
      if ((boisson == 1) && (sucre == 4) && (machines(nbmachine - 1).coffeeStocks < 8)) {
        return false
      }
      if ((boisson == 1) && (sucre == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 1) && (sucre == 3) && (machines(nbmachine - 1).sugarStocks < 10)) {
        return false
      }
      if ((boisson == 1) && (sucre == 4) && (machines(nbmachine - 1).sugarStocks < 15)) {
        return false
      }
      if ((boisson == 2) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).milkStocks >= 100)) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).milkStocks -= 100
        return true
      }
      if ((boisson == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 100)) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 5
        machines(nbmachine - 1).milkStocks -= 100
        return true
      }
      if ((boisson == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 10) && (machines(nbmachine - 1).milkStocks >= 100)) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 10
        machines(nbmachine - 1).milkStocks -= 100
        return true
      }
      if ((boisson == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 15) && (machines(nbmachine - 1).milkStocks >= 100)) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 15
        machines(nbmachine - 1).milkStocks -= 100
        return true
      }
      if ((boisson == 2) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).milkStocks >= 100 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).milkStocks -= 100 + (50 * dose)
        return true
      }
      if ((boisson == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 100 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 5
        machines(nbmachine - 1).milkStocks -= 100 + (50 * dose)
        return true
      }
      if ((boisson == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 10) && (machines(nbmachine - 1).milkStocks >= 100 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 10
        machines(nbmachine - 1).milkStocks -= 100 + (50 * dose)
        return true
      }
      if ((boisson == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 15) && (machines(nbmachine - 1).milkStocks >= 100 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 15
        machines(nbmachine - 1).milkStocks -= 100 + (50 * dose)
        return true
      }
      if ((boisson == 2) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 2) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 10)) {
        return false
      }
      if ((boisson == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 15)) {
        return false
      }
      if ((boisson == 2) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 100)) {
        return false
      }
      if ((boisson == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 100)) {
        return false
      }
      if ((boisson == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 100)) {
        return false
      }
      if ((boisson == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 100)) {
        return false
      }
      if ((boisson == 2) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 100 + (50 * dose))) {
        return false
      }
      if ((boisson == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 100 + (50 * dose))) {
        return false
      }
      if ((boisson == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 100 + (50 * dose))) {
        return false
      }
      if ((boisson == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 100 + (50 * dose))) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).milkStocks >= 120)) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).milkStocks -= 120
        return true
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).milkStocks >= 120)) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 5
        machines(nbmachine - 1).milkStocks -= 120
        return true
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).milkStocks >= 120)) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 10
        machines(nbmachine - 1).milkStocks -= 120
        return true
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).milkStocks >= 120)) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 15
        machines(nbmachine - 1).milkStocks -= 120
        return true
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 120 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).milkStocks -= 120 + (50 * dose)
        return true
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 120 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 5
        machines(nbmachine - 1).milkStocks -= 120 + (50 * dose)
        return true
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 120 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 10
        machines(nbmachine - 1).milkStocks -= 120 + (50 * dose)
        return true
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 120 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 15
        machines(nbmachine - 1).milkStocks -= 120 + (50 * dose)
        return true
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }

      if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }

      if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }

      if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }

      if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 120)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 120)) {
        return false
      }

      if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 120)) {
        return false
      }

      if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 120)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 120 + (50 * dose))) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 120 + (50 * dose))) {
        return false
      }

      if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 120 + (50 * dose))) {
        return false
      }

      if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 120 + (50 * dose))) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 150)) {
        machines(nbmachine - 1).coffeeStocks -= 8
        machines(nbmachine - 1).milkStocks -= 150
        return true
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 150)) {
        machines(nbmachine - 1).coffeeStocks -= 8
        machines(nbmachine - 1).sugarStocks -= 5
        machines(nbmachine - 1).milkStocks -= 150
        return true
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 150)) {
        machines(nbmachine - 1).coffeeStocks -= 8
        machines(nbmachine - 1).sugarStocks -= 10
        machines(nbmachine - 1).milkStocks -= 150
        return true
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 150)) {
        machines(nbmachine - 1).coffeeStocks -= 8
        machines(nbmachine - 1).sugarStocks -= 15
        machines(nbmachine - 1).milkStocks -= 150
        return true
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 150 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 8
        machines(nbmachine - 1).milkStocks -= 150 + (50 * dose)
        return true
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 150 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 8
        machines(nbmachine - 1).sugarStocks -= 5
        machines(nbmachine - 1).milkStocks -= 150 + (50 * dose)
        return true
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 150 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 8
        machines(nbmachine - 1).sugarStocks -= 10
        machines(nbmachine - 1).milkStocks -= 150 + (50 * dose)
        return true
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 150 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 8
        machines(nbmachine - 1).sugarStocks -= 15
        machines(nbmachine - 1).milkStocks -= 150 + (50 * dose)
        return true
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 8)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 8)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 8)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 8)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 8)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 8)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 8)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 8)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 150)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 150)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 150)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 150)) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 150 + (50 * dose))) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 150 + (50 * dose))) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 150 + (50 * dose))) {
        return false
      }
      if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 150 + (50 * dose))) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 200)) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).milkStocks -= 200
        return true
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 200)) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 5
        machines(nbmachine - 1).milkStocks -= 200
        return true
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 200)) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 10
        machines(nbmachine - 1).milkStocks -= 200
        return true
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 200)) {
        machines(nbmachine - 1).coffeeStocks -= 6
        machines(nbmachine - 1).sugarStocks -= 15
        machines(nbmachine - 1).milkStocks -= 200
        return true
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).milkStocks >= 200 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 12
        machines(nbmachine - 1).milkStocks -= 200 + (50 * dose)
        return true
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).milkStocks >= 200 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 12
        machines(nbmachine - 1).sugarStocks -= 5
        machines(nbmachine - 1).milkStocks -= 200 + (50 * dose)
        return true
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).milkStocks >= 200 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 12
        machines(nbmachine - 1).sugarStocks -= 10
        machines(nbmachine - 1).milkStocks -= 200 + (50 * dose)
        return true
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).milkStocks >= 200 + (50 * dose))) {
        machines(nbmachine - 1).coffeeStocks -= 12
        machines(nbmachine - 1).sugarStocks -= 15
        machines(nbmachine - 1).milkStocks -= 200 + (50 * dose)
        return true
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 12)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 12)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 12)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 12)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 12)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 12)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 12)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 12)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 5)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 200)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 200)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 200)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 200)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 200)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 200)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 200)) {
        return false
      }
      if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 200)) {
        return false
      }
      else return false
    }

    def affichage(): Unit = {
      println("Machine " + nbmachine + " chargée :" + "\n" + "   ID : " + nbmachine + "\n" + "   Code PIN : " + code + "\n" + "   Lait : " + (milkStocks.toDouble / 1000) + "L" + "\n" + "   Sucre : " + sugarStocks + "g" + "\n" + "   Café : " + coffeeStocks + "g" + "\n")
    }

    def imprimerCSV(): String = {
      return code + "," + milkStocks + "," + sugarStocks + "," + coffeeStocks
    }
  }

  machines = loadcsv("machines.csv")

  def serveClient(): Unit = {
    var p1 = Random.alphanumeric(1).toString
    var p2 = Random.alphanumeric(1).toString
    var p3 = Random.alphanumeric(1).toString
    var p4 = Random.alphanumeric(1).toString
    var p5 = Random.alphanumeric(1).toString
    var paiement = p1 + p2 + p3 + p4 + p5

    do {
      boisson = readLine("        Veuillez sélectionner votre boisson  \n" + "1) Expresso - CHF 2.00  \n" + "2) Cappuccino - CHF 2.50 \n" + "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)" + "\n").toInt
    } while (!(boisson == 1 || boisson == 2 || boisson == 3))
    if (boisson == 3) {
      do {
        latte = readLine("1) Petit \n" + "2) Moyen \n" + "3) Grand" + "\n").toInt
      } while (!(latte == 1 || latte == 2 || latte == 3))
    }
    do {
      sucre = readLine("Souhaitez-vous ajouter du sucre ?  \n" + "1) Sans sucre \n" + "2) Peu (5g) - CHF 0.10  \n" + "3) Moyen (10g) - CHF 0.20  \n" + "4) Beaucoup (15g) - CHF 0.30" + "\n").toInt
    } while (!(sucre == 1 || sucre == 2 || sucre == 3 || sucre == 4))
    if ((boisson == 2) || (boisson == 3)) {
      do {
        lait = readLine("Souhaitez-vous ajouter du lait en supplément ? \n" + "1) Oui \n" + "2) Non" + "\n").toInt
        if (lait == 1) {
          do {
            dose = readLine("Combien de dose ? (Dose - CHF 0.05, maximum 3 doses)" + "\n").toInt
          } while (!(dose == 1 || dose == 2 || dose == 3))
        }
      } while (!(lait == 1 || lait == 2))
    }

    // Expresso ( valable )
    if ((boisson == 1) && (sucre == 1) && (machines(nbmachine - 1).coffeeStocks >= 8)) {
      var prixfinal = prixE
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Sans sucre \n" + "Prix total : CHF %.2f \n" + " \n", prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Expresso est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 1) && (sucre == 2) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 5)) {
      var prixfinal = prixE + sucreP
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Peu (5g) \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixE, sucreP, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Expresso est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 1) && (sucre == 3) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 10)) {
      var prixfinal = prixE + sucreM
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Moyen (10g) \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixE, sucreM, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Expresso est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 1) && (sucre == 4) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 15)) {
      var prixfinal = prixE + sucreB
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixE, sucreB, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Expresso est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Expresso ( avec erreur de cafe )
    else if ((boisson == 1) && (sucre == 1) && (machines(nbmachine - 1).coffeeStocks < 8)) {
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Sans sucre \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 1) && (sucre == 2) && (machines(nbmachine - 1).coffeeStocks < 8)) {
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Peu (5g) \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 1) && (sucre == 3) && (machines(nbmachine - 1).coffeeStocks < 8)) {
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Moyen (10g) \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 1) && (sucre == 4) && (machines(nbmachine - 1).coffeeStocks < 8)) {
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Beaucoup (15g) \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Expresso ( avec erreur de sucre )
    else if ((boisson == 1) && (sucre == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Peu (5g) \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 1) && (sucre == 3) && (machines(nbmachine - 1).sugarStocks < 10)) {
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Moyen (10g) \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 1) && (sucre == 4) && (machines(nbmachine - 1).sugarStocks < 15)) {
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Beaucoup (15g) \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Cappuccino ( valable)
    if ((boisson == 2) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).milkStocks >= 100)) {
      var prixfinal = prixC
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f \n" + " \n", prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 100)) {
      var prixfinal = prixC + sucreP
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixC, sucreP, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 10) && (machines(nbmachine - 1).milkStocks >= 100)) {
      var prixfinal = prixC + sucreM
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + "\n", prixC, sucreM, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 15) && (machines(nbmachine - 1).milkStocks >= 100)) {
      var prixfinal = prixC + sucreB
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixC, sucreB, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).milkStocks >= 100 + (50 * dose))) {
      var prixfinal = prixC + (prixLait * dose)
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixC, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 100 + (50 * dose))) {
      var prixfinal = prixC + sucreP + (prixLait * dose)
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixC, sucreP, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 10) && (machines(nbmachine - 1).milkStocks >= 100 + (50 * dose))) {
      var prixfinal = prixC + sucreM + (prixLait * dose)
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixC, sucreM, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 15) && (machines(nbmachine - 1).milkStocks >= 100 + (50 * dose))) {
      var prixfinal = prixC + sucreB + (prixLait * dose)
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixC, sucreB, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Cappuccino ( avec erreur de cafe)
    if ((boisson == 2) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Cappuccino ( avec erreur de sucre)
    if ((boisson == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 10)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 15)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 5)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 10)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 15)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Cappuccino ( avec erreur de lait)
    if ((boisson == 2) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 100)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 100)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 100)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 100)) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 100 + (50 * dose))) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 100 + (50 * dose))) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 100 + (50 * dose))) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 100 + (50 * dose))) {
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Latte Petit ( valable)
    if ((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).milkStocks >= 120)) {
      var prixfinal = prixLP
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f \n" + " \n", prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 120)) {
      var prixfinal = prixLP + sucreP
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n", prixLP, sucreP, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 10) && (machines(nbmachine - 1).milkStocks >= 120)) {
      var prixfinal = prixLP + sucreM
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n", prixLP, sucreM, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 15) && (machines(nbmachine - 1).milkStocks >= 120)) {
      var prixfinal = prixLP + sucreB
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n", prixLP, sucreB, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).milkStocks >= 120 + (50 * dose))) {
      var prixfinal = prixLP + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n", prixLP, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 120 + (50 * dose))) {
      var prixfinal = prixLP + sucreP + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n", prixLP, sucreP, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 10) && (machines(nbmachine - 1).milkStocks >= 120 + (50 * dose))) {
      var prixfinal = prixLP + sucreM + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n", prixLP, sucreM, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 6) && (machines(nbmachine - 1).sugarStocks >= 15) && (machines(nbmachine - 1).milkStocks >= 120 + (50 * dose))) {
      var prixfinal = prixLP + sucreB + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n", prixLP, sucreB, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // latte petit ( avec probleme de cafe)
    if ((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 6)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Latte petit ( avec erreur de sucre)
    if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 10)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 15)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 5)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 10)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 15)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Latte Petit ( avec erreur de lait)
    if ((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 120)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 120)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 120)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 120)) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 120 + (50 * dose))) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 120 + (50 * dose))) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 120 + (50 * dose))) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 120 + (50 * dose))) {
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Latte Moyen ( valable )
    if ((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).milkStocks >= 150)) {
      var prixfinal = prixLM
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f \n" + " \n", prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 150)) {
      var prixfinal = prixLM + sucreP
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLM, sucreP, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 10) && (machines(nbmachine - 1).milkStocks >= 150)) {
      var prixfinal = prixLM + sucreM
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLM, sucreM, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 15) && (machines(nbmachine - 1).milkStocks >= 150)) {
      var prixfinal = prixLM + sucreB
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLM, sucreB, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).milkStocks >= 150 + (50 * dose))) {
      var prixfinal = prixLM + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLM, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 150 + (50 * dose))) {
      var prixfinal = prixLM + sucreP + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLM, sucreP, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 10) && (machines(nbmachine - 1).milkStocks >= 150 + (50 * dose))) {
      var prixfinal = prixLM + sucreM + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLM, sucreM, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 8) && (machines(nbmachine - 1).sugarStocks >= 15) && (machines(nbmachine - 1).milkStocks >= 150 + (50 * dose))) {
      var prixfinal = prixLM + sucreB + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLM, sucreB, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // latte moyen ( avec probleme de cafe)
    if ((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 8)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 8)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 8)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 8)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 8)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 8)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 8)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 8)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Latte moyen ( avec erreur de sucre)
    if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 10)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 15)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 5)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 10)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 15)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Latte moyen ( avec erreur de lait)
    if ((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 150)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 150)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 150)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 150)) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 150 + (50 * dose))) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 150 + (50 * dose))) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 150 + (50 * dose))) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 150 + (50 * dose))) {
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Latte Grand ( valable)
    if ((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).milkStocks >= 200)) {
      var prixfinal = prixLG
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f \n" + " \n", prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 200)) {
      var prixfinal = prixLG + sucreP
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLG, sucreP, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).sugarStocks >= 10) && (machines(nbmachine - 1).milkStocks >= 200)) {
      var prixfinal = prixLG + sucreM
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLG, sucreM, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).sugarStocks >= 15) && (machines(nbmachine - 1).milkStocks >= 200)) {
      var prixfinal = prixLG + sucreB
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLG, sucreB, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).milkStocks >= 200 + (50 * dose))) {
      var prixfinal = prixLG + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLG, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).sugarStocks >= 5) && (machines(nbmachine - 1).milkStocks >= 200 + (50 * dose))) {
      var prixfinal = prixLG + sucreP + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLG, sucreP, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).sugarStocks >= 10) && (machines(nbmachine - 1).milkStocks >= 200 + (50 * dose))) {
      var prixfinal = prixLG + sucreM + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLG, sucreM, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks >= 12) && (machines(nbmachine - 1).sugarStocks >= 15) && (machines(nbmachine - 1).milkStocks >= 200 + (50 * dose))) {
      var prixfinal = prixLG + sucreB + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLG, sucreB, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // latte grand ( avec probleme de cafe)
    if ((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 12)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 12)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 12)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).coffeeStocks < 12)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 12)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 12)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 12)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).coffeeStocks < 12)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Latte grand ( avec erreur de sucre)
    if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 5)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 10)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).sugarStocks < 15)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 5)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 10)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).sugarStocks < 15)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }

    // Latte grand ( avec erreur de lait)
    if ((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 200)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 200)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 200)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (machines(nbmachine - 1).milkStocks < 200)) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 200 + (50 * dose))) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 200 + (50 * dose))) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 200 + (50 * dose))) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (machines(nbmachine - 1).milkStocks < 200 + (50 * dose))) {
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" + ") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      machines(nbmachine - 1).removeIngredient(0, "")
    }
  }

  def loadcsv(file: String): ArrayBuffer[Machine] = {

    try {
      val filename = Source.fromFile(file)
      val lignefilename = filename.reset.getLines
      val ligne = lignefilename.next
      val mach = new ArrayBuffer[Machine]()
      var i = 0

      while (!lignefilename.isEmpty) {
        val ligne = lignefilename.next
        val unemachine = ligne.split(",")

        mach += new Machine()

        mach(i).nbmachine = i + 1
        mach(i).code = unemachine(0)
        mach(i).milkStocks = unemachine(1).toInt
        mach(i).sugarStocks = unemachine(2).toInt
        mach(i).coffeeStocks = unemachine(3).toInt
        i += 1
      }
      return mach
    }
    catch {
      case ex: java.io.FileNotFoundException => println("Fichier introuvable")
        System.exit(1)
        return null
    }
  }

  def saveCSV(file: String, machines: ArrayBuffer[Machine]): Unit = {
    val printWriter = new PrintWriter(new FileWriter("machines.csv", false))
    for (i <- 0 until machines.length) {
      printWriter.println(machines(i).imprimerCSV())
    }
    printWriter.close
  }

  def validatePin(): Boolean = {
    if (machines(nbmachine -1).code == mdp) {
      return true
    }
    else return false
  }

  def updatePin(): Unit = {
    println("Mise à jour du code PIN pour la machine " + nbmachine)
    do {
      machines(nbmachine - 1).code = readLine("Entrez un nouveau code Pin à 6 chiffres >")
    } while ((machines(nbmachine - 1).code.length) != 6)
    println("Le code PIN à été mis à jour avec succès. \n" + "Retour au menu principal...")
  }

  def main(args: Array[String]): Unit = {

    var option = 0
    var boucle = 1
    var commande = true


    println("Chargement des machines depuis machines.csv")
    var machinesMain = loadcsv("machines.csv")

    while (boucle == 1) {
      // Lancement

      for (i <- 0 until machines.length) {
        machines(i).affichage
      }
      do {
        mode = readLine("        Nosepresso Café  \n" + "Veuillez sélectionner votre mode :  \n" + "1) Client  \n" + "2) Admin  \n" + "3) Quittez" + "\n").toInt
        if (mode == 1) {
          do {
            nbmachine = readLine("Machine sélectionnée (1-5) ? >").toInt
          } while (!(nbmachine == 1 || nbmachine == 2 || nbmachine == 3 || nbmachine == 4 || nbmachine == 5))
          do {
            serveClient()
          } while (commande != true)
        }

        else if (mode == 2){
          var tentative = 2
          do{
            nbmachine = readLine("Machine sélectionnée (1-5) ? >").toInt
          }while(!(nbmachine == 1 || nbmachine == 2 || nbmachine == 3 || nbmachine == 4 || nbmachine == 5))
          mdp = readLine("Entrez le code PIN >")
          if(validatePin() != true){
            do{
              tentative -= 1
              mdp = readLine("Code PIN incoreect. " + (tentative + 1 ) + " tentatives restantes.")
            }while(tentative != 0 && validatePin() != true)
          }
          if(validatePin() == true ){
            tentative += 1
            println("Accès à la machine " + nbmachine + " accordé")
            do{
              option = readLine("Que voulez-vous faire ? \n" + "1) Réapprovisionner le stock \n" + "2) Changer le PIN de la machine").toInt
            }while(!(option == 1 || option == 2))
            if(option == 1){
              println("Niveaux de stock actuels : \n" + "   Poudre de café : " + machines(nbmachine - 1).coffeeStocks + " grammes"+ "\n" + "   Lait           : " + (machines(nbmachine -1).milkStocks.toDouble / 1000 ) + " Litre"+ "\n" + "   Sucre          : " + machines(nbmachine -1).sugarStocks + " grammes \n")
              machinesMain(nbmachine-1).addIngredient(0,"")
            }
            if(option == 2){
              updatePin()
            }
          }
          if( tentative == 0 ){
            println("Code PIN incorrect. 0 tentatives restantes. \n" + "Trop de tentatives échouées. Fin du programme.")
            boucle = 0
          }
        }
        else if (mode == 3){
          println("Merci ! Au revoir !")
          boucle = 0
          saveCSV("machines.csv",machines)
        }
      }while (!(mode == 1 || mode == 2 || mode == 3))
    }
  }
}