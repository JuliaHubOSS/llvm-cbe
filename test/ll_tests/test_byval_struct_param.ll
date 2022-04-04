; Test byval parameter attribute works.

%struct.intbox = type { i32 }

; Function Attrs: noinline nounwind optnone ssp uwtable
define void @add(%struct.intbox* noalias sret %0, %struct.intbox* byval(%struct.intbox) align 4 %1, %struct.intbox* byval(%struct.intbox) align 4 %2) #0 {
  %4 = getelementptr inbounds %struct.intbox, %struct.intbox* %1, i32 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = getelementptr inbounds %struct.intbox, %struct.intbox* %2, i32 0, i32 0
  %7 = load i32, i32* %6, align 4
  %8 = add nsw i32 %5, %7
  %9 = getelementptr inbounds %struct.intbox, %struct.intbox* %0, i32 0, i32 0
  store i32 %8, i32* %9, align 4
  ret void
}

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main(i32 %0, i8** %1) #0 {
  %3 = alloca %struct.intbox, align 4
  %4 = getelementptr inbounds %struct.intbox, %struct.intbox* %3, i32 0, i32 0
  store i32 2, i32* %4, align 4
  %5 = alloca %struct.intbox, align 4
  %6 = getelementptr inbounds %struct.intbox, %struct.intbox* %5, i32 0, i32 0
  store i32 4, i32* %6, align 4
  %7 = alloca %struct.intbox, align 4
  call void @add(%struct.intbox* sret %7, %struct.intbox* byval(%struct.intbox) align 4 %3, %struct.intbox* byval(%struct.intbox) align 4 %5)
  %8 = getelementptr inbounds %struct.intbox, %struct.intbox* %7, i32 0, i32 0
  %9 = load i32, i32* %8, align 4
  ret i32 %9
}
