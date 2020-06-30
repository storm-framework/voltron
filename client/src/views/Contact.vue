<template>
  <b-container>
    <div class="page-header">
      <br />
      <b-row>
        <b-col lg="12" md="8" sm="4">
          <h2 class="d-inline">Contact</h2>
        </b-col>
      </b-row>
    </div>
    <br />
    <b-row>
      <b-col lg="12" md="8" sm="4">
        <li>Email: {{ email }}</li>
        <li>Website: {{ web }}</li>
      </b-col>
    </b-row>
  </b-container>
</template>

<script>
export default {
  name: "Contact",
  data() {
    return {
      email: "rjhala@eng.ucsd.edu",
      web: "https://ranjitjhala.github.io"
    };
  }
};
</script>
