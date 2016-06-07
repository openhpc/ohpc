/ {
  Group: group1 {
    Dataset: dset1
    Group: group3 {
      Dataset: dset2
      Group: group4 {
        Group: group1 {
          Group: group5 {
            Warning: Loop detected!
          }
        }
        Group: group2 {
        }
      }
    }
  }
  Group: group2 {
    Dataset: dset2
    Group: group4 {
      Group: group1 {
        Group: group5 {
          Dataset: dset1
          Group: group3 {
            Warning: Loop detected!
          }
        }
      }
      Group: group2 {
      }
    }
  }
}
