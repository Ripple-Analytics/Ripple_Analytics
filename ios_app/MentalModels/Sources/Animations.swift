import SwiftUI

// MARK: - Animation System (Steve Jobs Level Polish)
/// Smooth, professional animations with M&S + Costco design language

// MARK: - Animation Constants
struct AnimationConstants {
    // Timing
    static let microDuration: Double = 0.15
    static let shortDuration: Double = 0.25
    static let mediumDuration: Double = 0.35
    static let longDuration: Double = 0.5
    
    // Spring
    static let defaultSpring = Animation.spring(response: 0.35, dampingFraction: 0.7)
    static let snappySpring = Animation.spring(response: 0.25, dampingFraction: 0.8)
    static let bouncySpring = Animation.spring(response: 0.4, dampingFraction: 0.6)
    static let gentleSpring = Animation.spring(response: 0.5, dampingFraction: 0.85)
    
    // Easing
    static let easeOut = Animation.easeOut(duration: shortDuration)
    static let easeIn = Animation.easeIn(duration: shortDuration)
    static let easeInOut = Animation.easeInOut(duration: mediumDuration)
}

// MARK: - Fade Animations
struct FadeInModifier: ViewModifier {
    @State private var opacity: Double = 0
    let delay: Double
    
    func body(content: Content) -> some View {
        content
            .opacity(opacity)
            .onAppear {
                withAnimation(AnimationConstants.easeOut.delay(delay)) {
                    opacity = 1
                }
            }
    }
}

struct FadeOutModifier: ViewModifier {
    @Binding var isVisible: Bool
    
    func body(content: Content) -> some View {
        content
            .opacity(isVisible ? 1 : 0)
            .animation(AnimationConstants.easeOut, value: isVisible)
    }
}

// MARK: - Slide Animations
struct SlideInFromBottomModifier: ViewModifier {
    @State private var offset: CGFloat = 50
    @State private var opacity: Double = 0
    let delay: Double
    
    func body(content: Content) -> some View {
        content
            .offset(y: offset)
            .opacity(opacity)
            .onAppear {
                withAnimation(AnimationConstants.defaultSpring.delay(delay)) {
                    offset = 0
                    opacity = 1
                }
            }
    }
}

struct SlideInFromTopModifier: ViewModifier {
    @State private var offset: CGFloat = -30
    @State private var opacity: Double = 0
    let delay: Double
    
    func body(content: Content) -> some View {
        content
            .offset(y: offset)
            .opacity(opacity)
            .onAppear {
                withAnimation(AnimationConstants.defaultSpring.delay(delay)) {
                    offset = 0
                    opacity = 1
                }
            }
    }
}

struct SlideInFromLeftModifier: ViewModifier {
    @State private var offset: CGFloat = -30
    @State private var opacity: Double = 0
    let delay: Double
    
    func body(content: Content) -> some View {
        content
            .offset(x: offset)
            .opacity(opacity)
            .onAppear {
                withAnimation(AnimationConstants.defaultSpring.delay(delay)) {
                    offset = 0
                    opacity = 1
                }
            }
    }
}

struct SlideInFromRightModifier: ViewModifier {
    @State private var offset: CGFloat = 30
    @State private var opacity: Double = 0
    let delay: Double
    
    func body(content: Content) -> some View {
        content
            .offset(x: offset)
            .opacity(opacity)
            .onAppear {
                withAnimation(AnimationConstants.defaultSpring.delay(delay)) {
                    offset = 0
                    opacity = 1
                }
            }
    }
}

// MARK: - Scale Animations
struct ScaleInModifier: ViewModifier {
    @State private var scale: CGFloat = 0.8
    @State private var opacity: Double = 0
    let delay: Double
    
    func body(content: Content) -> some View {
        content
            .scaleEffect(scale)
            .opacity(opacity)
            .onAppear {
                withAnimation(AnimationConstants.snappySpring.delay(delay)) {
                    scale = 1
                    opacity = 1
                }
            }
    }
}

struct PulseModifier: ViewModifier {
    @State private var scale: CGFloat = 1
    let isActive: Bool
    
    func body(content: Content) -> some View {
        content
            .scaleEffect(scale)
            .onChange(of: isActive) { active in
                if active {
                    withAnimation(Animation.easeInOut(duration: 0.8).repeatForever(autoreverses: true)) {
                        scale = 1.05
                    }
                } else {
                    withAnimation(AnimationConstants.snappySpring) {
                        scale = 1
                    }
                }
            }
    }
}

// MARK: - Press Effect
struct PressEffectModifier: ViewModifier {
    @State private var isPressed = false
    
    func body(content: Content) -> some View {
        content
            .scaleEffect(isPressed ? 0.96 : 1)
            .opacity(isPressed ? 0.9 : 1)
            .animation(AnimationConstants.snappySpring, value: isPressed)
            .simultaneousGesture(
                DragGesture(minimumDistance: 0)
                    .onChanged { _ in
                        if !isPressed {
                            isPressed = true
                            HapticManager.shared.impact(.light)
                        }
                    }
                    .onEnded { _ in
                        isPressed = false
                    }
            )
    }
}

// MARK: - Shimmer Effect (Loading State)
struct ShimmerModifier: ViewModifier {
    @State private var phase: CGFloat = 0
    let isActive: Bool
    
    func body(content: Content) -> some View {
        content
            .overlay(
                GeometryReader { geometry in
                    if isActive {
                        LinearGradient(
                            gradient: Gradient(colors: [
                                Color.clear,
                                Color.white.opacity(0.3),
                                Color.clear
                            ]),
                            startPoint: .leading,
                            endPoint: .trailing
                        )
                        .frame(width: geometry.size.width * 2)
                        .offset(x: -geometry.size.width + phase * geometry.size.width * 2)
                        .onAppear {
                            withAnimation(Animation.linear(duration: 1.5).repeatForever(autoreverses: false)) {
                                phase = 1
                            }
                        }
                    }
                }
                .mask(content)
            )
    }
}

// MARK: - Staggered List Animation
struct StaggeredListModifier: ViewModifier {
    let index: Int
    let baseDelay: Double
    let staggerDelay: Double
    
    @State private var offset: CGFloat = 20
    @State private var opacity: Double = 0
    
    func body(content: Content) -> some View {
        content
            .offset(y: offset)
            .opacity(opacity)
            .onAppear {
                let delay = baseDelay + (Double(index) * staggerDelay)
                withAnimation(AnimationConstants.defaultSpring.delay(delay)) {
                    offset = 0
                    opacity = 1
                }
            }
    }
}

// MARK: - Metric Counter Animation
struct AnimatedCounterModifier: ViewModifier {
    let value: Int
    @State private var displayedValue: Int = 0
    
    func body(content: Content) -> some View {
        content
            .onAppear {
                animateValue(to: value)
            }
            .onChange(of: value) { newValue in
                animateValue(to: newValue)
            }
    }
    
    private func animateValue(to target: Int) {
        let steps = 20
        let stepDuration = 0.5 / Double(steps)
        let increment = (target - displayedValue) / steps
        
        for i in 0..<steps {
            DispatchQueue.main.asyncAfter(deadline: .now() + stepDuration * Double(i)) {
                if i == steps - 1 {
                    displayedValue = target
                } else {
                    displayedValue += increment
                }
            }
        }
    }
}

// MARK: - Learning Indicator Animation
struct LearningPulseModifier: ViewModifier {
    let isLearning: Bool
    @State private var scale: CGFloat = 1
    @State private var opacity: Double = 1
    
    func body(content: Content) -> some View {
        ZStack {
            if isLearning {
                content
                    .scaleEffect(scale)
                    .opacity(opacity)
                    .onAppear {
                        withAnimation(Animation.easeOut(duration: 1.5).repeatForever(autoreverses: false)) {
                            scale = 2
                            opacity = 0
                        }
                    }
            }
            
            content
        }
    }
}

// MARK: - Accent Highlight Animation
struct AccentHighlightModifier: ViewModifier {
    let isHighlighted: Bool
    @State private var glowOpacity: Double = 0
    
    func body(content: Content) -> some View {
        content
            .overlay(
                RoundedRectangle(cornerRadius: 4)
                    .stroke(Color(red: 0.8, green: 0.1, blue: 0.1), lineWidth: 2)
                    .opacity(glowOpacity)
            )
            .onChange(of: isHighlighted) { highlighted in
                if highlighted {
                    withAnimation(Animation.easeInOut(duration: 0.3).repeatCount(2, autoreverses: true)) {
                        glowOpacity = 1
                    }
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.6) {
                        glowOpacity = 0
                    }
                }
            }
    }
}

// MARK: - View Extensions
extension View {
    // Fade
    func fadeIn(delay: Double = 0) -> some View {
        modifier(FadeInModifier(delay: delay))
    }
    
    func fadeOut(isVisible: Binding<Bool>) -> some View {
        modifier(FadeOutModifier(isVisible: isVisible))
    }
    
    // Slide
    func slideInFromBottom(delay: Double = 0) -> some View {
        modifier(SlideInFromBottomModifier(delay: delay))
    }
    
    func slideInFromTop(delay: Double = 0) -> some View {
        modifier(SlideInFromTopModifier(delay: delay))
    }
    
    func slideInFromLeft(delay: Double = 0) -> some View {
        modifier(SlideInFromLeftModifier(delay: delay))
    }
    
    func slideInFromRight(delay: Double = 0) -> some View {
        modifier(SlideInFromRightModifier(delay: delay))
    }
    
    // Scale
    func scaleIn(delay: Double = 0) -> some View {
        modifier(ScaleInModifier(delay: delay))
    }
    
    func pulse(isActive: Bool) -> some View {
        modifier(PulseModifier(isActive: isActive))
    }
    
    // Press
    func pressEffect() -> some View {
        modifier(PressEffectModifier())
    }
    
    // Shimmer
    func shimmer(isActive: Bool) -> some View {
        modifier(ShimmerModifier(isActive: isActive))
    }
    
    // Staggered
    func staggeredAnimation(index: Int, baseDelay: Double = 0, staggerDelay: Double = 0.05) -> some View {
        modifier(StaggeredListModifier(index: index, baseDelay: baseDelay, staggerDelay: staggerDelay))
    }
    
    // Learning
    func learningPulse(isLearning: Bool) -> some View {
        modifier(LearningPulseModifier(isLearning: isLearning))
    }
    
    // Accent
    func accentHighlight(isHighlighted: Bool) -> some View {
        modifier(AccentHighlightModifier(isHighlighted: isHighlighted))
    }
}

// MARK: - Animated Number View
struct AnimatedNumber: View {
    let value: Int
    let font: Font
    let color: Color
    
    @State private var displayedValue: Int = 0
    
    var body: some View {
        Text("\(displayedValue)")
            .font(font)
            .foregroundColor(color)
            .onAppear {
                animateValue(to: value)
            }
            .onChange(of: value) { newValue in
                animateValue(to: newValue)
            }
    }
    
    private func animateValue(to target: Int) {
        let steps = 20
        let duration = 0.5
        let stepDuration = duration / Double(steps)
        let startValue = displayedValue
        let difference = target - startValue
        
        for i in 0...steps {
            DispatchQueue.main.asyncAfter(deadline: .now() + stepDuration * Double(i)) {
                let progress = Double(i) / Double(steps)
                let easedProgress = 1 - pow(1 - progress, 3) // Ease out cubic
                displayedValue = startValue + Int(Double(difference) * easedProgress)
            }
        }
    }
}

// MARK: - Loading Skeleton View
struct SkeletonView: View {
    let width: CGFloat?
    let height: CGFloat
    
    @State private var phase: CGFloat = 0
    
    var body: some View {
        RoundedRectangle(cornerRadius: 4)
            .fill(Color(white: 0.9))
            .frame(width: width, height: height)
            .overlay(
                GeometryReader { geometry in
                    LinearGradient(
                        gradient: Gradient(colors: [
                            Color.clear,
                            Color.white.opacity(0.5),
                            Color.clear
                        ]),
                        startPoint: .leading,
                        endPoint: .trailing
                    )
                    .frame(width: geometry.size.width)
                    .offset(x: -geometry.size.width + phase * geometry.size.width * 2)
                }
                .mask(RoundedRectangle(cornerRadius: 4))
            )
            .onAppear {
                withAnimation(Animation.linear(duration: 1.2).repeatForever(autoreverses: false)) {
                    phase = 1
                }
            }
    }
}

// MARK: - Transition Definitions
extension AnyTransition {
    static var slideAndFade: AnyTransition {
        .asymmetric(
            insertion: .move(edge: .trailing).combined(with: .opacity),
            removal: .move(edge: .leading).combined(with: .opacity)
        )
    }
    
    static var scaleAndFade: AnyTransition {
        .asymmetric(
            insertion: .scale(scale: 0.9).combined(with: .opacity),
            removal: .scale(scale: 1.1).combined(with: .opacity)
        )
    }
    
    static var bottomSheet: AnyTransition {
        .asymmetric(
            insertion: .move(edge: .bottom).combined(with: .opacity),
            removal: .move(edge: .bottom).combined(with: .opacity)
        )
    }
}

// MARK: - Preview
struct AnimationsPreview: View {
    @State private var showContent = false
    @State private var isLearning = true
    @State private var counter = 0
    
    var body: some View {
        VStack(spacing: 20) {
            // Fade in example
            Text("Fade In")
                .fadeIn(delay: 0.1)
            
            // Slide in example
            Text("Slide In")
                .slideInFromBottom(delay: 0.2)
            
            // Scale in example
            Text("Scale In")
                .scaleIn(delay: 0.3)
            
            // Press effect example
            Button("Press Me") {
                counter += 10
            }
            .pressEffect()
            
            // Animated counter
            AnimatedNumber(
                value: counter,
                font: .system(size: 24, weight: .bold, design: .monospaced),
                color: Color(red: 0.8, green: 0.1, blue: 0.1)
            )
            
            // Learning pulse
            Circle()
                .fill(Color(red: 0.8, green: 0.1, blue: 0.1))
                .frame(width: 10, height: 10)
                .learningPulse(isLearning: isLearning)
            
            // Skeleton loading
            SkeletonView(width: 200, height: 20)
        }
        .padding()
    }
}

#Preview {
    AnimationsPreview()
}
