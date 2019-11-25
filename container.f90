Module container_module

  Implicit None

  Integer, Parameter, Private :: wp = Selected_real_kind( 12, 70 )

  Type, Public, Abstract :: container
   Contains
     Generic, Public :: Assignment( = ) => put_real
     Generic, Public :: Assignment( = ) => put_complex
     Generic, Public :: Assignment( = ) => get_real
     Generic, Public :: Assignment( = ) => get_complex
     Procedure, Pass( A ), Private :: put_real    => put_real
     Procedure, Pass( A ), Private :: put_complex => put_complex
     Procedure, Pass( A ), Private :: get_real    => get_real
     Procedure, Pass( A ), Private :: get_complex => get_complex
  End type container

  Type, Public, Extends( container ) :: real_container
     Character( Len = 8 ) :: data_held = "Real"
     Real( wp ) :: data
   Contains
     Procedure, Pass( A ), Private :: put_real => put_real_into_real
     Procedure, Pass( A ), Private :: get_real => get_real_from_real
  End type real_container

  Type, Public, Extends( container ) :: complex_container
     Complex( wp ) :: data
   Contains
     Procedure, Pass( A ), Private :: put_complex => put_complex_into_complex
     Procedure, Pass( A ), Private :: get_complex => get_complex_from_complex
  End type complex_container

  Private

Contains

  Impure Elemental Subroutine put_real( A, data )

    Class( container ), Intent( InOut ) :: A
    Real( wp )        , Intent( In    ) :: data

    Stop "Trying to put real data into a container that can't hold it"
    
  End Subroutine put_real
  
  Impure Elemental Subroutine put_complex( A, data )

    Class( container ), Intent( InOut ) :: A
    Complex( wp )     , Intent( In    ) :: data

    Stop "Trying to put complex data into a container that can't hold it"
    
  End Subroutine put_complex

  Impure Elemental Subroutine get_real( data, A )

    Real( wp )        , Intent( InOut ) :: data
    Class( container ), Intent( In    ) :: A

    Stop "Trying to get real data from a container that can't hold it"
    
  End Subroutine get_real
  
  Impure Elemental Subroutine get_complex( data, A )

    Complex( wp )     , Intent( InOut ) :: data
    Class( container ), Intent( In    ) :: A

    Stop "Trying to get complex data from a container that can't hold it"
    
  End Subroutine get_complex

  Elemental Subroutine put_real_into_real( A, data )

    Class( real_container ), Intent( InOut ) :: A
    Real( wp )             , Intent( In    ) :: data

    A%data = data
    
  End Subroutine put_real_into_real
  
  Elemental Subroutine get_real_from_real( data, A )

    Real( wp )             , Intent( InOut ) :: data
    Class( real_container ), Intent( In    ) :: A

    data = A%data
    
  End Subroutine get_real_from_real
  
  Elemental Subroutine put_complex_into_complex( A, data )

    Class( complex_container ), Intent( InOut ) :: A
    Complex( wp )             , Intent( In    ) :: data

    A%data = data
    
  End Subroutine put_complex_into_complex
  
  Elemental Subroutine get_complex_from_complex( data, A )

    Complex( wp )             , Intent( InOut ) :: data
    Class( complex_container ), Intent( In    ) :: A

    data = A%data
    
  End Subroutine get_complex_from_complex
  
End Module container_module

Module ks_result_info_module

  Use container_module

  Implicit None
  
  Integer, Parameter :: wp = Selected_real_kind( 12, 70 )

  Type, Public, Abstract :: ks_result_info_base
     Character( Len = 8 ) :: data_type = "None"
   Contains
     Generic, Public :: Assignment( = ) => put_real
     Generic, Public :: Assignment( = ) => put_complex
     Generic, Public :: Assignment( = ) => get_real
     Generic, Public :: Assignment( = ) => get_complex
     Procedure( put_real    ), Deferred, Pass( A ), Private :: put_real
     Procedure( put_complex ), Deferred, Pass( A ), Private :: put_complex
     Procedure( get_real    ), Deferred, Pass( A ), Private :: get_real
     Procedure( get_complex ), Deferred, Pass( A ), Private :: get_complex
  End type ks_result_info_base

  Type, Public, Extends( ks_result_info_base ) :: ks_result_info_scalar
     Class( container ), Allocatable :: data
   Contains
     Procedure, Pass( A ), Private :: put_real    => put_real_into_scalar
     Procedure, Pass( A ), Private :: put_complex => put_complex_into_scalar
     Procedure, Pass( A ), Private :: get_real    => get_real_from_scalar
     Procedure, Pass( A ), Private :: get_complex => get_complex_from_scalar
  End type ks_result_info_scalar

  Type, Public, Extends( ks_result_info_base ) :: ks_result_info_1D
     Class( container ), Dimension( : ), Allocatable :: data
   Contains
     Procedure, Pass( A ), Private :: put_real    => put_real_into_1D
     Procedure, Pass( A ), Private :: put_complex => put_complex_into_1D
     Procedure, Pass( A ), Private :: get_real    => get_real_from_1D
     Procedure, Pass( A ), Private :: get_complex => get_complex_from_1D
  End type ks_result_info_1D

  Private
  
Contains

  ! Dummy --------------------------------!
  
  Subroutine put_real( A, data )

    Class( ks_result_info_base ), Intent( InOut ) :: A
    Real( wp )                  , Intent( In    ) :: data

    Stop "Trying to put real data into a ks_result_info_base that can't hold it"

  End Subroutine put_real

  Subroutine put_complex( A, data )

    Class( ks_result_info_base ), Intent( InOut ) :: A
    Complex( wp )               , Intent( In    ) :: data

    Stop "Trying to put complex data into a ks_result_info_base that can't hold it"
    
  End Subroutine put_complex

   Subroutine get_real( data, A )

    Real( wp )                 , Intent( InOut ) :: data
    Class( ks_result_info_base ), Intent( In    ) :: A

    Stop "Trying to get real data from a ks_result_info_base that can't hold it"
    
  End Subroutine get_real
  
   Subroutine get_complex( data, A )

    Complex( wp )               , Intent( InOut ) :: data
    Class( ks_result_info_base ), Intent( In    ) :: A

    Stop "Trying to get complex data from a ks_result_info_base that can't hold it"
    
  End Subroutine get_complex

  ! Scalar --------------------------------!

  Subroutine put_real_into_scalar( A, data )

    Class( ks_result_info_scalar ), Intent( InOut ) :: A
    Real( wp )                    , Intent( In    ) :: data

    Allocate( real_container :: A%data )
    
    A%data_type = "Real"
    A%data = data

  End Subroutine put_real_into_scalar

  Subroutine put_complex_into_scalar( A, data )

    Class( ks_result_info_scalar ), Intent( InOut ) :: A
    Complex( wp )                 , Intent( In    ) :: data

    Allocate( complex_container :: A%data )

    A%data_type = "Complex"
    A%data = data

  End Subroutine put_complex_into_scalar

  Subroutine get_real_from_scalar( data, A )

    Real( wp )                    , Intent( InOut ) :: data
    Class( ks_result_info_scalar ), Intent( In    ) :: A

    data = A%data

  End Subroutine get_real_from_scalar
  
  Subroutine get_complex_from_scalar( data, A )

    Complex( wp )                 , Intent( InOut ) :: data
    Class( ks_result_info_scalar ), Intent( In    ) :: A

    data = A%data

  End Subroutine get_complex_from_scalar
  
  ! 1D Array --------------------------------!

  Subroutine put_real_into_1D( A, data )

    Class( ks_result_info_1D ), Intent( InOut ) :: A
    Real( wp ), Dimension( : )                    , Intent( In    ) :: data

    Allocate( real_container :: A%data )
    
    A%data_type = "Real"
    A%data = data

  End Subroutine put_real_into_1D

  Subroutine put_complex_into_1D( A, data )

    Class( ks_result_info_1D ), Intent( InOut ) :: A
    Complex( wp ), Dimension( : )                 , Intent( In    ) :: data

    Allocate( complex_container :: A%data )

    A%data_type = "Complex"
    A%data = data

  End Subroutine put_complex_into_1D

  Subroutine get_real_from_1D( data, A )

    Real( wp ), Dimension( : )                    , Intent( InOut ) :: data
    Class( ks_result_info_1D ), Intent( In    ) :: A

    data = A%data

  End Subroutine get_real_from_1D
  
  Subroutine get_complex_from_1D( data, A )

    Complex( wp ), Dimension( : )                 , Intent( InOut ) :: data
    Class( ks_result_info_1D ), Intent( In    ) :: A

    data = A%data

  End Subroutine get_complex_from_1D
  
End Module ks_result_info_module
  

Program driver

  Use container_module

  Implicit None
  
  Integer, Parameter :: wp = Selected_real_kind( 12, 70 )

  Complex( wp ) :: x, y
  
  Real( wp ) :: a, b

  Real( wp ), Dimension( : ), Allocatable :: ad, bd

  Class( container ), Allocatable :: c

  Class( container ), Dimension( : ), Allocatable :: cd

  Allocate( real_container :: c )
  a = 3.0_wp
  c = a
  b = c
  Write( *, * ) b
  Deallocate( c )
  
  Allocate( complex_container :: c )
  x = ( 5.0_wp, 6.1_wp )
  c = x
  y = c
  Write( *, * ) y
  Deallocate( c )
  
  Allocate( real_container :: cd( 1:3 ) )
  a  = 3.0_wp
  cd = a
  Allocate( bd( 1:size( cd ) ) )
  bd = cd
  Write( *, * ) bd
  Deallocate( cd )

End Program driver
