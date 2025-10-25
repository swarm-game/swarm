def modifyCart = \cartType. \device.
    result <- scan forward;
    case result pure (\item.
        if (item == cartType) {
            use device forward;
            pure ();
        } {
            pure ();
        };
    );
    end;

def watchSpot =
    watch forward;
    wait 1000;
    modifyCart "full cart" "fork";
    end;

def go =
    watchSpot;
    go;
    end;

go;
