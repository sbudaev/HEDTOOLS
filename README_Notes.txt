The design of the modules should be hirrarchical. Probably, they should go to
separate files:

1. Basic modules with broad applicability

  1.1. basic modules that have broad applicability to different models
       * BASE_UTILS.f90 -- various basic utils
       * BASE_STRINGS.f90 -- string manipulations (outer module)

  1.2. basic modules that have broad applicability but specific to certain task, 
       e.g. CSV output
       * BASE_CSV_IO.f90 -- Output data to CSV
       * BASE_LOGGER.f90 -- Log messages from the model

2. modules that are used in particular model (or a few models)
       * MODEL_UTILS_FISH_HED24.f90 -- anything like MMDD tags etc for
         specific fish model



