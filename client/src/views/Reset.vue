<template>
  <div class="reset">
    <b-form class="form-reset text-center" @submit.prevent="onSubmit">
      <br />
      <h3 class="mb-8">Reset your password</h3>
      <br />
      <b-form-input
        id="email-address"
        type="email"
        v-model="emailAddress"
        required
        placeholder="Email address"
      ></b-form-input>

      <b-form-invalid-feedback :state="isValid">
        Incorrect email address or password.
      </b-form-invalid-feedback>

      <b-button
        :disabled="loading"
        variant="primary"
        block
        size="lg"
        type="submit"
        class="mt-4"
      >
        Reset
      </b-button>
      <br />
    </b-form>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { ResetInfo } from "@/types";
import ApiService from "@/services/api";

@Component
export default class Reset extends Vue {
  emailAddress = "";
  password = "";
  loading = false;

  onSubmit() {
    this.loading = true;
    const reset: ResetInfo = {
      emailAddress: this.emailAddress
    };
    ApiService.reset(reset)
      .then(() => {
        const msg = "Please check your email for a reset link";
        this.showMessage(msg, "Thanks!", "success");
      })
      .catch(() => {
        const msg = "There is no account associated with that address!";
        this.showMessage(msg, "Sorry!", "danger");
      });
  }

  showMessage(msg: string, title: string, variant: string) {
    this.$bvToast.toast(msg, {
      toaster: "b-toaster-top-center",
      solid: true,
      title,
      variant
    });
  }
}
</script>

<style lang="scss">
.reset {
  width: 100%;
  display: flex;
  align-items: center;
}

.form-reset {
  width: 100%;
  max-width: 350px;
  padding: 15px;
  margin: 0 auto;
}

.form-reset input[type="email"] {
  margin-bottom: -1px;
  border-bottom-right-radius: 0;
  border-bottom-left-radius: 0;
}

.form-reset input[type="password"] {
  margin-bottom: 10px;
  border-top-left-radius: 0;
  border-top-right-radius: 0;
}
.form-reset .form-control {
  height: auto;
  position: relative;
  box-sizing: border-box;
  padding: 10px;
  font-size: 16px;
}
</style>
