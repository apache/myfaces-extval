/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.core.interceptor;

import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ValidationModuleKey;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.storage.RendererInterceptorPropertyStorage;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipBeforeInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipRendererDelegationException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipAfterInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.RendererProxy;
import org.apache.myfaces.extensions.validator.core.recorder.ProcessedInformationRecorder;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.component.EditableValueHolder;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import javax.faces.validator.ValidatorException;
import javax.el.PropertyNotFoundException;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;
import java.util.logging.Level;

/**
 * A basic implementation of {@link RendererInterceptor} for validating fields.
 * It adds some extension point for subclasses and performs tasks like : <br/>
 * - storing field values ({@link #recordProcessedInformation}) <br/>
 * - resetting required information property UIComponent <br/>
 * - calling before and after Validation interceptors <br/>
 * - etc ...
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.REUSE)
public abstract class AbstractValidationInterceptor extends AbstractRendererInterceptor
{
    private ELHelper elHelper;

    /**
     * In case of required initialization
     * it's needed for some use-cases to reset the (required-)state of the components.
     * Such a reset is just needed if required initialization is activated.
     *
     * @return true if required initialization is supported by the current implementation, false otherwise
     */
    protected boolean isRequiredInitializationSupported()
    {
        return false;
    }

    /**
     * Sets required property of UIComponent to false after decoding.
     * It's needed for special use-cases if required initialization is supported.
     * The final required validation will be done by the corresponding constraint validator.
     *
     * {@inheritDoc}
     */
    @Override
    public void afterDecode(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws SkipAfterInterceptorsException
    {
        /*
         * component initialization sets a component to required if there are constraints which indicate it
         * the required flag in a component leads to problems with h:messages (additional message) as well as
         * incompatibilities with skip validation and severities
         */
        if(uiComponent instanceof EditableValueHolder && ExtValUtils.isRequiredResetActivated() &&
                isRequiredInitializationSupported() && ExtValUtils.isRequiredInitializationActive())
        {
            ((EditableValueHolder)uiComponent).setRequired(false);
        }
    }

    /**
     * Before the component gets rendered the interceptor initializes the component based on the meta-data
     * which is provided by the referenced property (if component initialization is activated).
     *
     * {@inheritDoc}
     */
    @Override
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        if(processComponent(uiComponent) && !isComponentInitializationDeactivated())
        {
            initComponent(facesContext, uiComponent);
        }
    }

    /**
     * Initialize the component based on the meta-data which is provided by the referenced property.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The component which is processed
     */
    protected abstract void initComponent(FacesContext facesContext, UIComponent uiComponent);

    /**
     * The method performs the validation of the field and calls the registered interceptors regarding the validation.
     *
     * The main steps are :<br/>
     * - Get the converted value from the renderer (possibly cached by the RendererProxy)<br/>
     * - Record the value (e.g. for cross validation)<br/>
     * - Adjust the converted value for interpret empty values as null.<br/>
     * - Execute the beforeValidation method of the registered PropertyValidationInterceptor's. <br/>
     * - Perform the validation when the PropertyValidationInterceptor have indicate it that it should be performed.
     * <br/>
     * - When a validation error occurred, ask the ViolationSeverityInterpreter if this validation should result in an
     *  exception <br/>
     * - Execute the afterValidation method of the registered PropertyValidationInterceptor's. (when validation actually
     *  tooks place)
     *
     * {@inheritDoc}
     */
    @Override
    public void beforeGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o, Renderer wrapped)
            throws ConverterException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        Object convertedObject;

        try
        {
            if(wrapped instanceof RendererProxy)
            {
                convertedObject = ((RendererProxy)wrapped).getCachedConvertedValue(facesContext, uiComponent, o);
            }
            else
            {
                convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);
            }
        }
        catch (PropertyNotFoundException r)
        {
            this.logger.log(Level.SEVERE, "it seems you are using an invalid binding. " + wrapped.getClass().getName()
                    + ": conversion failed. normally this is >not< a myfaces extval issue!", r);

            throw r;
        }

        setRendererInterceptorProperties(uiComponent);
        
        if(recordProcessedInformation())
        {
            //record user input e.g. for cross-component validation
            for(ProcessedInformationRecorder recorder : ExtValContext.getContext().getProcessedInformationRecorders())
            {
                recorder.recordUserInput(uiComponent, convertedObject);

                logger.finest(recorder.getClass().getName() + " called");
            }
        }

        boolean validateValue = false;
        try
        {
            if(processComponent(uiComponent))
            {
                convertedObject = transformValueForValidation(convertedObject);

                validateValue = validateValue(convertedObject);
                if(validateValue && processBeforeValidation(facesContext, uiComponent, convertedObject))
                {
                    processValidation(facesContext, uiComponent, convertedObject);
                }
            }
        }
        catch (ValidatorException e)
        {
            try
            {
                //ViolationSeverityInterpreter might decide that it isn't an exception
                ExtValUtils.tryToThrowValidatorExceptionForComponent(uiComponent, e.getFacesMessage(), e);
            }
            catch (ValidatorException finalException)
            {
                throw new ConverterException(e.getFacesMessage(), e);
            }
        }
        finally
        {
            if(validateValue)
            {
                processAfterValidation(facesContext, uiComponent, convertedObject);
            }
            resetRendererInterceptorProperties(uiComponent);
        }
    }

    /**
     * Execute the beforeValidation method of the registered PropertyValidationInterceptor's.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The UIComponent which is processed.
     * @param value The value to validate
     * @return true when validation can proceed, false otherwise.
     */
    protected boolean processBeforeValidation(FacesContext facesContext, UIComponent uiComponent, Object value)
    {
        return ExtValUtils.executeGlobalBeforeValidationInterceptors(facesContext, uiComponent, value,
                PropertyInformation.class.getName(), getPropertyInformation(facesContext, uiComponent), getModuleKey());
    }

    /**
     * Execute the afterValidation method of the registered PropertyValidationInterceptor's.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The UIComponent which is processed.
     * @param value The value which has just been validated.
     */
    protected void processAfterValidation(FacesContext facesContext, UIComponent uiComponent, Object value)
    {
        ExtValUtils.executeGlobalAfterValidationInterceptors(facesContext, uiComponent, value,
                PropertyInformation.class.getName(), getPropertyInformation(facesContext, uiComponent), getModuleKey());
    }

    /**
     * Extrancts the {@link PropertyInformation} for the given component.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The UIComponent which is processed.
     * @return the information of the referenced property (e.g. base object, property name, meta-data, ...)
     */
    protected PropertyInformation getPropertyInformation(FacesContext facesContext, UIComponent uiComponent)
    {
        Map<String, Object> properties = getPropertiesForComponentMetaDataExtractor(uiComponent);

        MetaDataExtractor metaDataExtractor = getComponentMetaDataExtractor(properties);

        return metaDataExtractor.extract(facesContext, uiComponent);
    }

    /**
     * Implementations must return the MetaDataExtractor that will perform the extraction of the meta data from the
     * component. The component itself is present in the properties map (it might influence the type of the returned
     * {@link MetaDataExtractor}.
     *
     * @param properties Properties that can be used to determine the extractor which is returned.
     * @return The MetaDataExtractor used for performing the xtraction of the meta data from the component.
     */
    protected abstract MetaDataExtractor getComponentMetaDataExtractor(Map<String, Object> properties);

    /**
     * Converts an empty String to null when the parameter interpretEmptyStringValuesAsNull is set.
     *
     * @param convertedObject  Converted objected
     * @return Adjusted value that should be used from now on as converted value.
     */
    protected Object transformValueForValidation(Object convertedObject)
    {
        if ("".equals(convertedObject) && interpretEmptyStringValuesAsNull())
        {
            return null;
        }

        return convertedObject;
    }

    /**
     * Evaluates if the value should be validated in case it is empty (null or no characters).
     * ExtVal also uses a config parameter introduced by JSF 2 which allows to skip the validation of empty fields.
     *
     * @param convertedObject The converted value.
     * @return true if the given value should be validated, false otherwise
     */
    protected boolean validateValue(Object convertedObject)
    {
        if(isValueToValidateEmpty(convertedObject) && !validateEmptyFields())
        {
            this.logger.fine("empty field validation is deactivated in the web.xml - see: " +
                    "javax.faces.VALIDATE_EMPTY_FIELDS");

            return false;
        }

        return true;
    }

    /**
     * Defines if a value is empty. The definition of empty is that it is null or has no characters in the String value.
     *
     * @param convertedObject The converted value.
     * @return true if the given value is the representation of an empty value
     */
    protected boolean isValueToValidateEmpty(Object convertedObject)
    {
        return convertedObject == null || "".equals(convertedObject);
    }

    /**
     * Uses a config parameter (javax.faces.VALIDATE_EMPTY_FIELDS) which was introduced by JSF 2 for deactivating
     * the validation of empty fields.
     * 
     * @return true if the validation of empty fields is enabled, false otherwise
     */
    protected boolean validateEmptyFields()
    {
        return ExtValUtils.validateEmptyFields();
    }

    /**
     * Uses a config parameter (javax.faces.INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL)
     * which was introduced by JSF 2 for converting empty strings to null.
     *
     * @return true if an empty string should be replaced with null (for the validation process), false otherwise
     */
    protected boolean interpretEmptyStringValuesAsNull()
    {
        return ExtValUtils.interpretEmptyStringValuesAsNull();
    }

    /**
     * A concrete implementation has to perform the actual validation of the value.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The UIComponent which is processed.
     * @param convertedObject The (adjusted) converted value.
     */
    protected abstract void processValidation(
            FacesContext facesContext, UIComponent uiComponent, Object convertedObject);

    /**
     * Based on basic rules the method checks if the current component should be processed.
     *
     * @param uiComponent The UIComponent which is processed.
     * @return true if the given component should be processed by the current interceptor, false otherwise
     */
    protected boolean processComponent(UIComponent uiComponent)
    {
        return uiComponent instanceof EditableValueHolder && isValueBindingOfComponentValid(uiComponent);
    }

    /**
     * Returns the ELHelper to be used in the process of the validation. It is cached for performance reasons.
     * @return The ELHelper.
     */
    protected ELHelper getELHelper()
    {
        if(this.elHelper == null)
        {
            this.elHelper = ExtValUtils.getELHelper();
        }
        return this.elHelper;
    }

    private boolean isValueBindingOfComponentValid(UIComponent uiComponent)
    {
        try
        {
            return getELHelper().getPropertyDetailsOfValueBinding(uiComponent) != null;
        }
        catch (Exception e)
        {
            return false;
        }
    }

    private boolean isComponentInitializationDeactivated()
    {
        return ExtValCoreConfiguration.get().deactivateComponentInitialization();
    }

    /**
     * Signals if the converted value should be stored e.g. for the cross-validation process.
     * @return true if the converted value should be recorded, false otherwise
     */
    protected boolean recordProcessedInformation()
    {
        //override if needed
        return false;
    }

    /**
     * Identification of the validation module.
     * A key is just needed if other implementations should be restricted to 1-n special validation modules.
     * 
     * @return Identification of the validation module.
     */
    protected Class getModuleKey()
    {
        //override if needed
        return null;
    }

    /**
     * Create the properties which can be used by a {@link MetaDataExtractor}.
     * By default it adds a key which identifies the current validation-module
     * and the current component (to avoid changes of the api for quite special use-cases).
     *
     * @param uiComponent  The UIComponent which is processed.
     * @return properties used by the selection of the MetaDataExtractor
     */
    protected Map<String, Object> getPropertiesForComponentMetaDataExtractor(UIComponent uiComponent)
    {
        return createProperties(uiComponent);
    }

    /**
     * Create the properties which can be used by a {@link MetaDataExtractor}.
     * Returns the properties which will be made available to interceptors. By default the moduleKey and the UIComponent
     * itself is added.
     *
     * @param uiComponent  The UIComponent which is processed.
     * @return  properties for the interceptors.
     */
    protected Map<String, Object> getInterceptorProperties(UIComponent uiComponent)
    {
        return createProperties(uiComponent);
    }

    private Map<String, Object> createProperties(UIComponent uiComponent)
    {
        Map<String, Object> result = new HashMap<String, Object>();

        if(getModuleKey() != null)
        {
            result.put(ValidationModuleKey.class.getName(), getModuleKey());
        }
        result.put(UIComponent.class.getName(), uiComponent);

        return result;
    }

    /**
     * Stores additional properties for the current process (to avoid api changes).
     *
     * @param uiComponent The UIComponent which is processed.
     */
    private void setRendererInterceptorProperties(UIComponent uiComponent)
    {
        RendererInterceptorPropertyStorage interceptorPropertyStorage = getRendererInterceptorPropertyStorage();

        Map<String, Object> properties = getInterceptorProperties(uiComponent);
        for(Map.Entry<String, Object> entry : properties.entrySet())
        {
            interceptorPropertyStorage.setProperty(entry.getKey(), entry.getValue());
        }
    }

    private void resetRendererInterceptorProperties(UIComponent uiComponent)
    {
        RendererInterceptorPropertyStorage interceptorPropertyStorage = getRendererInterceptorPropertyStorage();

        for(String key : getInterceptorProperties(uiComponent).keySet())
        {
            interceptorPropertyStorage.removeProperty(key);
        }
    }

    private RendererInterceptorPropertyStorage getRendererInterceptorPropertyStorage()
    {
        return ExtValUtils.getStorage(RendererInterceptorPropertyStorage.class,
                RendererInterceptorPropertyStorage.class.getName());
    }
}
