#![cfg_attr(not(feature = "std"), no_std)]

pub use pallet::*;
use scale_info::TypeInfo;
use sp_runtime::traits::SignedExtension;
use codec::{Decode, Encode};
use sp_std::marker::PhantomData;
use sp_runtime::{
	traits::{DispatchInfoOf, PostDispatchInfoOf},
	transaction_validity::{
		InvalidTransaction, TransactionValidity, TransactionValidityError, ValidTransaction,
	},};
use frame_support::traits::IsSubType;

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

#[cfg(feature = "runtime-benchmarks")]
mod benchmarking;

#[frame_support::pallet]
pub mod pallet {
	use frame_support::pallet_prelude::*;
	use frame_system::pallet_prelude::*;

	#[pallet::config]
	pub trait Config: frame_system::Config + pallet_uniques::Config {
		type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;
		#[pallet::constant]
		type Day: Get<u32>;
		#[pallet::constant]
		type Hour: Get<u32>;
	}

	#[pallet::pallet]
	#[pallet::generate_store(pub(super) trait Store)]
	pub struct Pallet<T>(_);

	#[pallet::storage]
	#[pallet::getter(fn get_timed_pass)]
	pub(super) type TimedPass<T: Config> = StorageMap<
		_,
		Blake2_128Concat,
		T::AccountId,
		PassInfo<<T as frame_system::Config>::BlockNumber>,
		OptionQuery
	>;

	#[pallet::event]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]
	pub enum Event<T: Config> {
		SomethingStored(u32, T::AccountId),
	}

	// Errors inform users that something went wrong.
	#[pallet::error]
	pub enum Error<T> {
		NoneValue,
		StorageOverflow,
	}

	#[derive(Debug, Encode, Decode, Clone, Eq, PartialEq, TypeInfo, MaxEncodedLen, PartialOrd)]
	pub enum PassType {
		// Usage(u8),   
		Day
		// TotalWeight,
	}

	#[derive(Encode, Decode, TypeInfo, MaxEncodedLen, PartialEq, Eq, PartialOrd)]
	pub struct PassInfo<BlockNumber> {
		pub pass_type: PassType,
		pub expiration: BlockNumber
	}

	pub trait Pass {
		/// Perform all checks required of a pass, deduct any held value(if any), and return the boolean result of verification
		fn check_pass_is_valid(&self) -> bool;
	}

	impl<T: Config + PartialOrd + frame_system::Config<BlockNumber = T>> Pass for PassInfo<T> {
		fn check_pass_is_valid(&self) -> bool {
			match &self.pass_type {
				&PassType::Day => {
					if <frame_system::Pallet<T>>::block_number() > self.expiration {
						false
					} else {
						true
					}
				},
				// Usage, TotalWeight... include any deductions
			}
		}
	}

	#[pallet::call]
	impl<T: Config> Pallet<T> {
		#[pallet::weight(10_000 + T::DbWeight::get().writes(1))]
		pub fn create_chain_hourly_pass(
			origin: OriginFor<T>,
			pass_type: PassType,
			owner: T::AccountId
		) -> DispatchResult {
			let who = ensure_signed(origin)?;

			let current_block: T::BlockNumber = <frame_system::Pallet<T>>::block_number().into();
			let expiration = current_block + T::Hour::get().into();
			TimedPass::<T>::insert(owner, PassInfo { pass_type, expiration });
			Ok(())
		}
	}
}

#[derive(Encode, Decode, Clone, Eq, PartialEq, TypeInfo)]
pub struct ChainPass<T: Config + Send + Sync>(PhantomData<T>);

impl<T: Config + Send + Sync> ChainPass<T> {
	pub fn new() -> Self {
		ChainPass(PhantomData)
	}
}

impl<T: Config + Send + Sync> sp_std::fmt::Debug for ChainPass<T> {
	fn fmt(&self, f: &mut sp_std::fmt::Formatter) -> sp_std::fmt::Result {
		write!(f, "WatchDummy")
	}
}


impl<T: Config + Send + Sync + TypeInfo> SignedExtension for ChainPass<T>
where
	T::Call: IsSubType<Call<T>>{
	const IDENTIFIER: &'static str = "CheckTxPass";
	type AccountId = T::AccountId;
	type Call = T::Call;
	type AdditionalSigned = ();
	type Pre = ();

	fn additional_signed(&self) -> sp_std::result::Result<(), TransactionValidityError> {
		Ok(())
	}

	fn validate(
		&self,
		who: &Self::AccountId,
		call: &Self::Call,
		_info: &DispatchInfoOf<Self::Call>,
		_len: usize,
	) -> TransactionValidity {
		match call.is_sub_type() {
			// If they want to create a pass itself, just ignore it and let it through for now. 
			Some(Call::create_chain_hourly_pass { .. }) => Ok(ValidTransaction { ..Default::default() }),
			// All other tx types should be checked
			_ => {
				// If there is no pass found, or if that pass is invalid, return bad tx for lack of "payment"
				match Pallet::<T>::get_timed_pass(who) {
					Some(pass) if pass.expiration < <frame_system::Pallet<T>>::block_number() => Ok(ValidTransaction { ..Default::default() }),
					_ => Err(TransactionValidityError::Invalid(InvalidTransaction::Payment))
				}
			}
		}
	}

	fn pre_dispatch(
		self,
		who: &Self::AccountId,
		call: &Self::Call,
		info: &DispatchInfoOf<Self::Call>,
		len: usize,
	) -> Result<Self::Pre, TransactionValidityError> {
		Self::validate(&self, who, call, info, len).map(|_| ())
	}

	fn post_dispatch(
		_pre: Option<Self::Pre>,
		_info: &DispatchInfoOf<Self::Call>,
		_post_info: &PostDispatchInfoOf<Self::Call>,
		_len: usize,
		_result: &sp_runtime::DispatchResult,
	) -> Result<(), TransactionValidityError> {
		
		// TODO check weight used and add it to the interim amount checked for the pass 
		Ok(())
	}

}