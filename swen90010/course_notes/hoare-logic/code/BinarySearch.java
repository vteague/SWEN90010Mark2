  int binarySearch(int [] list, int target)
  {
    int low = 0;
    int high = len(list) - 1;
    int mid;

    while(low <= high) {
      mid = (low + high)/2;
      if (list[mid] < target) {
        low = mid + 1;
      }
      else if (list[mid] > target) {
        high = mid - 1;
      }
      else {
        return mid;
      }
    }
    return -1;
  }
