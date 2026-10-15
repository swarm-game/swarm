def modifyCart = \cartType. \device.
    item <- scan forward;
    if (item == "") {} {
        if (item == cartType) {
            use device forward;
            pure ();
        } {
            pure ();
        };
    };
    end;

def watchSpot =
    watch forward;
    wait 1000;
    modifyCart "empty cart" "spatula";
    end;

def go =
    watchSpot;
    go;
    end;
