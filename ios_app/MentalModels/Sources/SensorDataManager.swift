import Foundation
import CoreMotion
import CoreLocation
import HealthKit
import Combine

class SensorDataManager: NSObject, ObservableObject, CLLocationManagerDelegate {
    @Published var isCollecting = false
    @Published var accelerometerData: CMAccelerometerData?
    @Published var gyroscopeData: CMGyroData?
    @Published var locationData: CLLocation?
    @Published var healthData: [String: Double] = [:]
    @Published var dataPointsCollected: Int = 0
    
    private let motionManager = CMMotionManager()
    private let locationManager = CLLocationManager()
    private let healthStore = HKHealthStore()
    private let pedometer = CMPedometer()
    private let altimeter = CMAltimeter()
    
    private var learningEngine: ContinuousLearningEngine?
    private var cancellables = Set<AnyCancellable>()
    
    private let sensorQueue = OperationQueue()
    
    override init() {
        super.init()
        sensorQueue.name = "com.ripple.sensors"
        sensorQueue.maxConcurrentOperationCount = 1
        
        locationManager.delegate = self
        locationManager.desiredAccuracy = kCLLocationAccuracyBest
        locationManager.allowsBackgroundLocationUpdates = true
        locationManager.pausesLocationUpdatesAutomatically = false
    }
    
    func setLearningEngine(_ engine: ContinuousLearningEngine) {
        self.learningEngine = engine
    }
    
    func startCollecting() {
        guard !isCollecting else { return }
        isCollecting = true
        
        startAccelerometer()
        startGyroscope()
        startLocationUpdates()
        startHealthKitUpdates()
        startPedometerUpdates()
        startAltimeterUpdates()
        startDeviceMotion()
    }
    
    func stopCollecting() {
        isCollecting = false
        
        motionManager.stopAccelerometerUpdates()
        motionManager.stopGyroUpdates()
        motionManager.stopDeviceMotionUpdates()
        locationManager.stopUpdatingLocation()
        pedometer.stopUpdates()
        altimeter.stopRelativeAltitudeUpdates()
    }
    
    private func startAccelerometer() {
        guard motionManager.isAccelerometerAvailable else { return }
        
        motionManager.accelerometerUpdateInterval = 0.1
        motionManager.startAccelerometerUpdates(to: sensorQueue) { [weak self] data, error in
            guard let self = self, let data = data, error == nil else { return }
            
            DispatchQueue.main.async {
                self.accelerometerData = data
                self.dataPointsCollected += 1
            }
            
            self.learningEngine?.addSensorData(
                source: "accelerometer",
                type: "motion",
                values: [
                    "x": data.acceleration.x,
                    "y": data.acceleration.y,
                    "z": data.acceleration.z,
                    "magnitude": sqrt(pow(data.acceleration.x, 2) + pow(data.acceleration.y, 2) + pow(data.acceleration.z, 2))
                ]
            )
        }
    }
    
    private func startGyroscope() {
        guard motionManager.isGyroAvailable else { return }
        
        motionManager.gyroUpdateInterval = 0.1
        motionManager.startGyroUpdates(to: sensorQueue) { [weak self] data, error in
            guard let self = self, let data = data, error == nil else { return }
            
            DispatchQueue.main.async {
                self.gyroscopeData = data
                self.dataPointsCollected += 1
            }
            
            self.learningEngine?.addSensorData(
                source: "gyroscope",
                type: "rotation",
                values: [
                    "x": data.rotationRate.x,
                    "y": data.rotationRate.y,
                    "z": data.rotationRate.z
                ]
            )
        }
    }
    
    private func startDeviceMotion() {
        guard motionManager.isDeviceMotionAvailable else { return }
        
        motionManager.deviceMotionUpdateInterval = 0.1
        motionManager.startDeviceMotionUpdates(to: sensorQueue) { [weak self] motion, error in
            guard let self = self, let motion = motion, error == nil else { return }
            
            self.dataPointsCollected += 1
            
            self.learningEngine?.addSensorData(
                source: "device_motion",
                type: "attitude",
                values: [
                    "pitch": motion.attitude.pitch,
                    "roll": motion.attitude.roll,
                    "yaw": motion.attitude.yaw,
                    "gravity_x": motion.gravity.x,
                    "gravity_y": motion.gravity.y,
                    "gravity_z": motion.gravity.z,
                    "user_accel_x": motion.userAcceleration.x,
                    "user_accel_y": motion.userAcceleration.y,
                    "user_accel_z": motion.userAcceleration.z
                ]
            )
        }
    }
    
    private func startLocationUpdates() {
        locationManager.requestAlwaysAuthorization()
        locationManager.startUpdatingLocation()
    }
    
    func locationManager(_ manager: CLLocationManager, didUpdateLocations locations: [CLLocation]) {
        guard let location = locations.last else { return }
        
        DispatchQueue.main.async {
            self.locationData = location
            self.dataPointsCollected += 1
        }
        
        learningEngine?.addSensorData(
            source: "location",
            type: "gps",
            values: [
                "latitude": location.coordinate.latitude,
                "longitude": location.coordinate.longitude,
                "altitude": location.altitude,
                "speed": location.speed,
                "course": location.course,
                "horizontal_accuracy": location.horizontalAccuracy,
                "vertical_accuracy": location.verticalAccuracy
            ]
        )
    }
    
    func locationManager(_ manager: CLLocationManager, didFailWithError error: Error) {
        print("Location error: \(error)")
    }
    
    private func startHealthKitUpdates() {
        guard HKHealthStore.isHealthDataAvailable() else { return }
        
        let typesToRead: Set<HKObjectType> = [
            HKObjectType.quantityType(forIdentifier: .heartRate)!,
            HKObjectType.quantityType(forIdentifier: .stepCount)!,
            HKObjectType.quantityType(forIdentifier: .activeEnergyBurned)!,
            HKObjectType.quantityType(forIdentifier: .distanceWalkingRunning)!,
            HKObjectType.quantityType(forIdentifier: .flightsClimbed)!,
            HKObjectType.quantityType(forIdentifier: .oxygenSaturation)!,
            HKObjectType.quantityType(forIdentifier: .respiratoryRate)!,
            HKObjectType.quantityType(forIdentifier: .bodyTemperature)!,
            HKObjectType.categoryType(forIdentifier: .sleepAnalysis)!
        ]
        
        healthStore.requestAuthorization(toShare: nil, read: typesToRead) { [weak self] success, error in
            if success {
                self?.startHeartRateQuery()
                self?.startStepCountQuery()
                self?.startActiveEnergyQuery()
            }
        }
    }
    
    private func startHeartRateQuery() {
        guard let heartRateType = HKObjectType.quantityType(forIdentifier: .heartRate) else { return }
        
        let query = HKObserverQuery(sampleType: heartRateType, predicate: nil) { [weak self] query, completionHandler, error in
            self?.fetchLatestHeartRate()
            completionHandler()
        }
        
        healthStore.execute(query)
    }
    
    private func fetchLatestHeartRate() {
        guard let heartRateType = HKObjectType.quantityType(forIdentifier: .heartRate) else { return }
        
        let sortDescriptor = NSSortDescriptor(key: HKSampleSortIdentifierStartDate, ascending: false)
        let query = HKSampleQuery(sampleType: heartRateType, predicate: nil, limit: 1, sortDescriptors: [sortDescriptor]) { [weak self] query, samples, error in
            guard let sample = samples?.first as? HKQuantitySample else { return }
            
            let heartRate = sample.quantity.doubleValue(for: HKUnit(from: "count/min"))
            
            DispatchQueue.main.async {
                self?.healthData["heartRate"] = heartRate
                self?.dataPointsCollected += 1
            }
            
            self?.learningEngine?.addSensorData(
                source: "health_kit",
                type: "heart_rate",
                values: ["bpm": heartRate]
            )
        }
        
        healthStore.execute(query)
    }
    
    private func startStepCountQuery() {
        guard let stepType = HKObjectType.quantityType(forIdentifier: .stepCount) else { return }
        
        let query = HKObserverQuery(sampleType: stepType, predicate: nil) { [weak self] query, completionHandler, error in
            self?.fetchTodaySteps()
            completionHandler()
        }
        
        healthStore.execute(query)
    }
    
    private func fetchTodaySteps() {
        guard let stepType = HKObjectType.quantityType(forIdentifier: .stepCount) else { return }
        
        let calendar = Calendar.current
        let now = Date()
        let startOfDay = calendar.startOfDay(for: now)
        let predicate = HKQuery.predicateForSamples(withStart: startOfDay, end: now, options: .strictStartDate)
        
        let query = HKStatisticsQuery(quantityType: stepType, quantitySamplePredicate: predicate, options: .cumulativeSum) { [weak self] query, result, error in
            guard let result = result, let sum = result.sumQuantity() else { return }
            
            let steps = sum.doubleValue(for: HKUnit.count())
            
            DispatchQueue.main.async {
                self?.healthData["steps"] = steps
                self?.dataPointsCollected += 1
            }
            
            self?.learningEngine?.addSensorData(
                source: "health_kit",
                type: "steps",
                values: ["count": steps]
            )
        }
        
        healthStore.execute(query)
    }
    
    private func startActiveEnergyQuery() {
        guard let energyType = HKObjectType.quantityType(forIdentifier: .activeEnergyBurned) else { return }
        
        let query = HKObserverQuery(sampleType: energyType, predicate: nil) { [weak self] query, completionHandler, error in
            self?.fetchTodayActiveEnergy()
            completionHandler()
        }
        
        healthStore.execute(query)
    }
    
    private func fetchTodayActiveEnergy() {
        guard let energyType = HKObjectType.quantityType(forIdentifier: .activeEnergyBurned) else { return }
        
        let calendar = Calendar.current
        let now = Date()
        let startOfDay = calendar.startOfDay(for: now)
        let predicate = HKQuery.predicateForSamples(withStart: startOfDay, end: now, options: .strictStartDate)
        
        let query = HKStatisticsQuery(quantityType: energyType, quantitySamplePredicate: predicate, options: .cumulativeSum) { [weak self] query, result, error in
            guard let result = result, let sum = result.sumQuantity() else { return }
            
            let calories = sum.doubleValue(for: HKUnit.kilocalorie())
            
            DispatchQueue.main.async {
                self?.healthData["activeEnergy"] = calories
                self?.dataPointsCollected += 1
            }
            
            self?.learningEngine?.addSensorData(
                source: "health_kit",
                type: "active_energy",
                values: ["calories": calories]
            )
        }
        
        healthStore.execute(query)
    }
    
    private func startPedometerUpdates() {
        guard CMPedometer.isStepCountingAvailable() else { return }
        
        pedometer.startUpdates(from: Date()) { [weak self] data, error in
            guard let data = data, error == nil else { return }
            
            self?.dataPointsCollected += 1
            
            var values: [String: Double] = [
                "steps": Double(truncating: data.numberOfSteps),
                "distance": data.distance?.doubleValue ?? 0
            ]
            
            if let pace = data.currentPace {
                values["pace"] = pace.doubleValue
            }
            if let cadence = data.currentCadence {
                values["cadence"] = cadence.doubleValue
            }
            if let floors = data.floorsAscended {
                values["floors_ascended"] = floors.doubleValue
            }
            if let floorsDesc = data.floorsDescended {
                values["floors_descended"] = floorsDesc.doubleValue
            }
            
            self?.learningEngine?.addSensorData(
                source: "pedometer",
                type: "activity",
                values: values
            )
        }
    }
    
    private func startAltimeterUpdates() {
        guard CMAltimeter.isRelativeAltitudeAvailable() else { return }
        
        altimeter.startRelativeAltitudeUpdates(to: sensorQueue) { [weak self] data, error in
            guard let data = data, error == nil else { return }
            
            self?.dataPointsCollected += 1
            
            self?.learningEngine?.addSensorData(
                source: "altimeter",
                type: "altitude",
                values: [
                    "relative_altitude": data.relativeAltitude.doubleValue,
                    "pressure": data.pressure.doubleValue
                ]
            )
        }
    }
}
