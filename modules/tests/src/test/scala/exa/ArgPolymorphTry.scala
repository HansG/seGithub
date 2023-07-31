package exa

object ArgPolymorphTry extends App {

  abstract class Vehicle(name: String)
  case class Car(name: String) extends Vehicle(name)
  case class Bike(name: String) extends Vehicle(name)

  trait VehicleInventoryService[V] { //<: Vehicle
    def checkStock(vehicle: V): Unit = {
      println(s"checking stock for vehicle = $vehicle")
    }
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

  trait VehicleSystem[V <: Vehicle] {
    this: VehicleServices[V] =>

    def buyVehicle(vehicle: V)(implicit vis :  VehicleInventoryService[V]): Unit = {
      println(s"buying vehicle $vehicle")
      vis.checkStock(vehicle)
      vehiclePricingService.checkPrice(vehicle)
    }
  }


  object VehicleApp extends VehicleSystem[Vehicle] with VehicleServices[Vehicle]

  VehicleApp.buyVehicle(Car("mazda 3 series"))(VehicleInventoryService4Car)
  VehicleApp.buyVehicle(Bike("honda bike firestorm"))





}
