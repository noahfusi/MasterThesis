import scala.io.StdIn._
import scala.util.Random

object Main {

  def pin(): String = {   
    Random.alphanumeric.take(5).mkString    
  }
  
  def main(args: Array[String]): Unit = {
    
    var action : Short = 0
    var actualPIN : String = "434343"
    var enteredPIN : String = "0"

    var stayinloop : Boolean = true

    var coffee : Long = 50
    var sugar : Long = 30
    var milk : Double = 0.5 

    var beverage_choice : Short = 0
    var sugar_choice : Short = 0
    var milk_choice : Short = 0
    var doses_choice : Short = 0
    var latte_size_choice : Short = 0

    var beverage_making : Boolean = false

    val word : String = pin()

    while(stayinloop == true) {
      while(action < 1 || action > 3) {
        
        doses_choice = 0
        
        println("")
        println("Nospresso Café")
        println("")
        println("Please Select your Mode :")
        println("")
        println("1) Client")
        println("2) Admin")
        println("3) Exit")
        print("> ")
        action = readShort()

        if(action < 1 || action > 3) {
          println("Please enter a valid number")
          println("")
        }
      }
    
      if(action == 1) {
        println("Please select your beverage : ")
        println("")
        println("1) Espresso - CHF 2.00")
        println("2) Cappucino - CHF 2.50")
        println("3) Latte - CHF 2.70 (small), CHF 3.20 (medium), CHF 3.70 (large)")
        print("> ")
        beverage_choice = readShort()

        if(beverage_choice == 1 || beverage_choice == 2) {
          println("Would you like to add sugar ? ")
          println("")
          println("1) No sugar")
          println("2) Light (5g), CHF 0.10")
          println("3) Medium (10g), CHF 0.20")
          println("4) Heavy (15g), CHF 0.30")
          print("> ")
          sugar_choice = readShort()
        }
        if(beverage_choice == 2) {
          println("Would you like to add milk ? ")
          println("")
          println("1) Yes")
          println("2) No")
          println("")
          print("> ")
          milk_choice = readShort()
          if(milk_choice == 1) {
            while(doses_choice < 1 || doses_choice > 3) {
              println("How many doses ? (up to 3 doses maximum) ")
              print("> ")
              doses_choice = readShort()
              if(doses_choice < 1 || doses_choice > 3) {
                println("Please select a number of doses between 1 and 3.")
              }
            }
          }
        }
        if(beverage_choice == 1) {
          if(coffee >= 8) {
            if(sugar_choice == 1) {
              println("The total price is CHF 2.00")
              beverage_making = true
            }
            if(sugar_choice == 2) {
              if(sugar >= 5) {
                sugar -= 5
                println("The total price is CHF 2.00 + CHf 0.10 = CHF 2.10")
                beverage_making = true
              } else {
                println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                action = 0
              }
            }
            if(sugar_choice == 3) {
              if(sugar >= 10) {
                sugar -= 10
                println("The total price is CHF 2.00 + CHf 0.20 = CHF 2.20")
                beverage_making = true
              } else {
                println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                action = 0
              }
            }
            if(sugar_choice == 4) {
              if(sugar >= 15) {
                sugar -= 15
                println("The total price is CHF 2.00 + CHf 0.30 = CHF 2.30")
                beverage_making = true
              } else {
                println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                action = 0
              }
            }
            coffee -= 8
          } else {
            println("Insufficient coffee powder to prepare the selected beverage. Please select another beverage or try a smaller size.")
            action = 0
          }
        }
        if(beverage_choice == 2) {
          if(coffee >= 6 || milk >= 0.1) {
            if(sugar_choice == 1) {
              if(milk_choice == 1) {
                println("The total price is CHF 2.50 + CHF " + (doses_choice * 0.05) + " = CHF " + (2.50 + (doses_choice * 0.05)))
                beverage_making = true
                milk -= doses_choice * 0.05
              } else {
                println("The total price is CHF 2.50")
                beverage_making = true
                milk -= doses_choice * 0.05
              }
            }
            if(sugar_choice == 2) {
              if(sugar >= 5) {
                if(milk_choice == 1) {
                  println("The total price is CHF 2.00 + CHf 0.10 + CHF " + (doses_choice * 0.05) + " = CHF " + (2.50 + 0.10 + (doses_choice * 0.05)))
                  beverage_making = true
                  sugar -= 5
                  milk -= doses_choice * 0.05
                } else {
                  println("The total price is CHF 2.50 + CHF 0.10 = CHF 2.60")
                  beverage_making = true
                  sugar -= 5
                  milk -= doses_choice * 0.05
                }
              } else {
                println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                action = 0
              }
            }
            if(sugar_choice == 3) {
              if(sugar >= 10) {
                if(milk_choice == 1) {
                  println("The total price is CHF 2.00 + CHf 0.20 + CHF " + (doses_choice * 0.05) + " = CHF " + (2.50 + 0.20 + (doses_choice * 0.05)))
                  beverage_making = true
                  sugar -= 10
                  milk -= doses_choice * 0.05
                } else {
                  println("The total price is CHF 2.50 + CHF 0.20 = CHF 2.70")
                  beverage_making = true
                  sugar -= 10
                  milk -= doses_choice * 0.05
                }
              } else {
                println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                action = 0
              }
            }
            if(sugar_choice == 4) {
              if(sugar >= 15) {
                if(milk_choice == 1) {
                  println("The total price is CHF 2.00 + CHf 0.30 + CHF " + (doses_choice * 0.05) + " = CHF " + (2.00 + 0.30 + (doses_choice * 0.05)))
                  beverage_making = true
                  sugar -= 15
                  milk -= doses_choice * 0.05
                }else {
                  println("The total price is CHF 2.50 + CHF 0.30 = CHF 2.80")
                  beverage_making = true
                  sugar -= 15
                  milk -= doses_choice * 0.05
                }
              } else {
                println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                action = 0
              }
            }
          coffee -= 6
          milk -= 0.1
          } else {
            if(coffee < 6) {
              println("Insufficient coffee powder to prepare the selected beverage. Please select another beverage or try a smaller size.")
            }
            if(milk < 0.1) {
              println("Insufficient milk to prepare the selected beverage. Please select another beverage or try a smaller size.")
            }
            action = 0
          }
        }
        if(beverage_choice == 3) {
          if(coffee >= 6 || milk >= 0.12) {
            println("Choose the size of your latte : ")
            println("")
            println("1) Small - CHF 2.70")
            println("2) Medium - CHF 3.20")
            println("3) Large - CHF 3.70")
            println("")
            print("> ")
            latte_size_choice = readShort()

            println("Would you like to add sugar ? ")
            println("")
            println("1) No sugar")
            println("2) Light (5g)")
            println("3) Medium (10g)")
            println("4) Heavy (15g)")
            println("")
            print("> ")
            sugar_choice = readShort()
            println("")
            println("Would you like to add milk ? ")
            println("")
            println("1) Yes")
            println("2) No")
            println("")
            print("> ")
            milk_choice = readShort()
            if(milk_choice == 1) {
              while(doses_choice < 1 || doses_choice > 3) {
                println("How many doses ? (up to 3 doses maximum) ")
                print("> ")
                doses_choice = readShort()
                if(doses_choice < 1 || doses_choice > 3) {
                  println("Please select a number of doses between 1 and 3.")
                }
              }
            } 
            if(latte_size_choice == 1) {
              if(sugar_choice == 1) {
                if(milk_choice == 1) {
                  println("The total price is CHF 2.50 + CHF " + (doses_choice * 0.05) + " = CHF " + (2.50 + (doses_choice * 0.05)))
                  beverage_making = true
                  milk -= doses_choice * 0.05
                } else {
                  println("The total price is CHF 2.50")
                  beverage_making = true
                }
              }
              if(sugar_choice == 2) {
                if(sugar >= 5) {
                  sugar -= 5
                  if(milk_choice == 1) {
                    println("The total price is CHF 2.70 + CHf 0.10 + CHF " + (doses_choice * 0.05) + " = CHF " + (2.70 + 0.10 + (doses_choice * 0.05)))
                    beverage_making = true
                    milk -= doses_choice * 0.05
                  } else {
                    println("The total price is CHF 2.70 + CHF 0.10 = CHF 2.80")
                    beverage_making = true
                  }
                } else {
                  println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                }
              }
              if(sugar_choice == 3) {
                if(sugar >= 10) {
                  sugar -= 10
                  if(milk_choice == 1) {
                    println("The total price is CHF 2.70 + CHf 0.20 + CHF " + (doses_choice * 0.05) + " = CHF " + (2.70 + 0.20 + (doses_choice * 0.05)))
                    beverage_making = true
                    milk -= doses_choice * 0.05
                  } else {
                    println("The total price is CHF 2.70 + CHF 0.20 = CHF 2.90")
                    beverage_making = true
                  }
                } else {
                println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                }
              }
              if(sugar_choice == 4) {
                if(sugar >= 15) {
                  sugar -= 15
                  if(milk_choice == 1) {
                    println("The total price is CHF 2.70 + CHf 0.30 + CHF " + (doses_choice * 0.05) + " = CHF " + (2.70 + 0.30 + (doses_choice * 0.05)))
                    beverage_making = true
                    milk -= doses_choice * 0.05
                  } else {
                    println("The total price is CHF 2.70 + CHF 0.30 = CHF 3.00")
                    beverage_making = true
                  }
                } else {
                  println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                }
              }
            coffee -= 6
            milk -= (0.12 + (doses_choice * 0.05))
            }
            if(latte_size_choice == 2) {
              if(coffee >= 8 || milk >= 0.15) {
                if(sugar_choice == 1) {
                  if(milk_choice ==1) {
                    println("The total price is CHF 3.20 + CHF " + (doses_choice * 0.05) + " = CHF " + (3.20 + (doses_choice * 0.05)))
                    beverage_making = true
                  } else {
                    println("The total price is CHF 3.20")
                    beverage_making = true
                  }
                }
                if(sugar_choice == 2) {
                  if(sugar >= 5) {
                    sugar -= 5
                    if(milk_choice == 1) {
                      println("The total price is CHF 3.20 + CHf 0.10 + CHF " + (doses_choice * 0.05) + " = CHF " + (3.20 + 0.10 + (doses_choice * 0.05)))
                      beverage_making = true
                      milk -= doses_choice * 0.05
                    } else {
                      println("The total price is CHF 3.20 + CHF 0.10 = CHF 3.30")
                      beverage_making = true
                    }
                  } else {
                  println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                  }
                }
                if(sugar_choice == 3) {
                  if(sugar >= 10) {
                    sugar -= 10
                    if(milk_choice == 1) {
                      println("The total price is CHF 3.20 + CHf 0.20 + CHF " + (doses_choice * 0.05) + " = CHF " + (3.20 + 0.20 + (doses_choice * 0.05)))
                      beverage_making = true
                      milk -= doses_choice * 0.05
                    } else {
                      println("The total price is CHF 3.20 + CHF 0.20 = CHF 3.40")
                      beverage_making = true
                    }
                  } else {
                    println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                  }
                }
                if(sugar_choice == 4) {
                  if(sugar >= 15) {
                    sugar -= 15
                    if(milk_choice == 1) {
                      println("The total price is CHF 3.20 + CHf 0.30 + CHF " + (doses_choice * 0.05) + " = CHF " + (3.20 + 0.30 + (doses_choice * 0.05)))
                      beverage_making = true
                      milk -= doses_choice * 0.05
                    } else {
                      println("The total price is CHF 3.20 + CHF 0.30 = CHF 3.50")
                      beverage_making = true
                    }
                  } else {
                    println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                  }
                }
              } 
            coffee -= 8
            milk -= (0.15 + (doses_choice * 0.05))
            }
            if(latte_size_choice == 3) {
              if(coffee >= 12 || milk >= 0.2) {
                if(sugar_choice == 1) {
                  if(milk_choice ==1) {
                    println("The total price is CHF 3.70 + CHF " + (doses_choice * 0.05) + " = CHF " + (3.20 + (doses_choice * 0.05)).toDouble)
                    beverage_making = true
                  } else {
                    println("The total price is CHF 3.70")
                    beverage_making = true
                  }
                }
                if(sugar_choice == 2) {
                  if(sugar >= 5) {
                    sugar -= 5
                    if(milk_choice == 1) {
                      println("The total price is CHF 3.70 + CHf 0.10 + CHF " + (doses_choice * 0.05) + " = CHF " + (3.70 + 0.10 + (doses_choice * 0.05)))
                      beverage_making = true
                      milk -= doses_choice * 0.05
                    } else {
                      println("The total price is CHF 3.70 + CHF 0.10 = CHF 3.80")
                      beverage_making = true
                    }
                  } else {
                    println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                  }
                }
                if(sugar_choice == 3) {
                  if(sugar >= 10) {
                    sugar -= 10
                    if(milk_choice == 1) {
                      println("The total price is CHF 3.70 + CHf 0.20 + CHF " + (doses_choice * 0.05) + " = CHF " + (3.70 + 0.20 + (doses_choice * 0.05)))
                      beverage_making = true
                      milk -= doses_choice * 0.05
                    } else {
                      println("The total price is CHF 3.70 + CHF 0.20 = CHF 3.90")
                      beverage_making = true
                    }
                  } else {
                    println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                  }
                }
                if(sugar_choice == 4) {
                  if(sugar >= 15) {
                    sugar -= 15
                    if(milk_choice == 1) {
                      println("The total price is CHF 3.70 + CHf 0.30 + CHF " + (doses_choice * 0.05) + " = CHF " + (3.70 + 0.30 + (doses_choice * 0.05)))
                      beverage_making = true
                      milk -= doses_choice * 0.05
                    } else {
                      println("The total price is CHF 3.70 + CHF 0.30 = CHF 4.00")
                      beverage_making = true
                    }
                  } else {
                    println("Insufficient sugar to prepare the selected beverage. Please select another beverage or try a smaller size.")
                  }
                }
              }
            coffee -= 12
            milk -= 0.2
            }
          action = 0
          } else {
            if(coffee < 6) {
              println("Insufficient coffee powder to prepare the selected beverage. Please select another beverage or try a smaller size.")
            }
            if(milk < 0.12) {
              println("Insufficient milk to prepare the selected beverage. Please select another beverage or try a smaller size.")
            }
            action = 0
          }
        }
        if(beverage_making == true) {
          println("")
          println("Please pay using TWINT:")
          println("")
          println("Your TWINT payment code is: " + word)
          println("")
          println("(Waiting for payment valisation...)")
          Thread.sleep(5000)
          println("")
          println("Payment confirmed")
          Thread.sleep(1000)
          println("")
          println("Preparing your beverage...")
          println("")
          Thread.sleep(3000)
          if(beverage_choice ==1) {
            println("Your espresso is ready! Enjoy!")
            println("")
          } else if(beverage_choice == 2) {
            println("Your cappucino is ready! Enjoy!")
            println("")
          } else if(beverage_choice == 3) {
            println("Your latte is ready! Enjoy!")
            println("")
          }
          action = 0
        }
        beverage_making = false
      }
      if(action == 2) {
        print("Please enter your PIN code > ")
        enteredPIN = readLine()
        if(enteredPIN == actualPIN) {
          println("")
          println("Current Stocks :")
          println("")
          println("Coffee : " + coffee + "g")
          println("Sugar : " + sugar + "g")
          println("Milk : " + milk + "ml")
          println("")
          println("Enter the new stocks")
          print("coffee : ")
          var c = readShort()
          print("sugar : ")
          var s = readShort()
          print("milk : ")
          var m = readShort()
          println("You have successfully added " + c + "g of coffee, " + s + "g of sugar, and " + m + "ml of milk to the stocks")
          println("")
          println("new stocks : ")
          println("")
          println("Coffee : " + (coffee + c) + "g")
          println("Sugar : " + (sugar + s) + "g")
          println("Milk : " + (milk + m) + "L")
          Thread.sleep(1000)
          println("")
          println("Returning to the main menu...")
          Thread.sleep(2000)
        } else {
          while(enteredPIN != actualPIN) {
            println("")
            println("Wrong PIN code, please try again")
            println("")
            print("Please enter your PIN code > ")
            enteredPIN = readLine()
          }
          if(enteredPIN == actualPIN) {
            println("")
            println("Current Stocks :")
            println("")
            println("Coffee : " + coffee + "g")
            println("Sugar : " + sugar + "g")              
            println("Milk : " + milk + "L")
            println("")
            println("Enter the amounts you want to add : ")
            print("coffee : ")
            var c = readLong()
            print("sugar : ")
            var s = readLong()
            print("milk : ")              
            var m = readDouble()
            println("")
            println("Stocks levels updated.")
            println("")
            println("You have successfully added " + c + "g of coffee, " + s + "g of sugar, and " + m + "ml of milk to the stocks")
            println("")
            println("new stocks : ")
            println("")
            println("Coffee : " + (coffee + c) + "g")
            println("Sugar : " + (sugar + s) + "g")
            println("Milk : " + (milk + m) + "L")
            coffee += c
            sugar += s
            milk += m
            Thread.sleep(1000)
            println("")
            println("Returning to the main menu...")
            Thread.sleep(2000)
          }
        }
        action = 0
      }
      if(action == 3) {
        stayinloop = false
        println("Exiting the program. Have a nice day!")
        println("")
      }
    }    
  }
}