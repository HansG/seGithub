package exa


/*
Polymorh nach Argument-Typ über Typeclass: suche Typeclass zu Parametertyp -> nehme Implementierung daraus
anders: self-type in https://allaboutscala.com/tutorials/scala-exercises-solutions/
 */
object ArgPolymorphTry extends App {

  abstract class Vehicle(name: String)
  case class Car(name: String) extends Vehicle(name)
  case class Bike(name: String) extends Vehicle(name)

  trait VehicleInventoryService[V <: Vehicle] { //
    def checkStock(vehicle: V): Unit = {
      println(s"checking stock for vehicle = $vehicle")
    }
  }

  object VehicleInventoryService {
    def apply[U <: Vehicle] (implicit ev : VehicleInventoryService[U] ): VehicleInventoryService[U] = ev
  }

  implicit object VehicleInventoryService4Car   extends VehicleInventoryService[Car]{
    override def checkStock(vehicle: Car): Unit = {
      println(s"checking stock for !Car! = $vehicle")
    }
  }

  implicit object  VehicleInventoryService4Bike   extends VehicleInventoryService[Bike]{
    override def checkStock(vehicle: Bike): Unit = {
      println(s"checking stock for !Bike! = $vehicle")
    }
  }

  class VehiclePricingService[V <: Vehicle] {
    def checkPrice(vehicle: V): Unit = {
      println(s"checking price for vehicle = $vehicle")
    }
  }

  trait VehicleServices[V <: Vehicle] {
    lazy val vehiclePricingService = new VehiclePricingService[V]
  }

  trait VehicleSystem {/*
  mit [V <: Vehicle]: auf Klasse/Trait-Ebene muss V vor Verwendung von Methoden festgelegt werden (auf Vehicle) -> Typclass muss Ableitung von  VehicleInventoryService[Vehicle]
  -> Varianz +V -> V darf nicht mehr in Argumentposition verwendet werden
  */
//    this: VehicleServices[V] =>  self-type: dazu wäre VehicleSystem[V <: Vehicle] nötig

    def buyVehicle[V <: Vehicle : VehicleInventoryService](vehicle: V) : Unit = {
      println(s"buying vehicle $vehicle")
      VehicleInventoryService[V].checkStock(vehicle)
//      vehiclePricingService.checkPrice(vehicle)
    }
  }


  object VehicleApp extends VehicleSystem  //with VehicleServices[Vehicle]

  VehicleApp.buyVehicle(Car("mazda 3 series"))
  VehicleApp.buyVehicle(Bike("honda bike firestorm"))





}
