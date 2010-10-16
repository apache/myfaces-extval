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
package org.apache.myfaces.extensions.validator.core.validation.strategy;

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.exception.RequiredValidatorException;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import java.util.logging.Logger;

/**
 * Provides the ability to use ValidatorException (as expected by the user) instead of ConverterException.
 * Furthermore it provides:<br/>
 * initValidation<br/>
 * processAfterValidatorException
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.REUSE})
public abstract class AbstractValidationStrategy implements ValidationStrategy
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    protected AbstractValidationStrategy()
    {
        logger.fine(getClass().getName() + " instantiated");
    }

    /**
     * {@inheritDoc}
     * Before actually executing the validation (done by the method processValidation) any initialization can be
     * performed by the overriding the initValidation method. When a Validation exception occurs, the method
     * processAfterValidatorException executes the registered
     * {@link org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor}'s.
     *
     */
    public void validate(FacesContext facesContext, UIComponent uiComponent,
                         MetaDataEntry metaDataEntry, Object convertedObject)
    {
        logger.finest("start initValidation of " + getClass().getName());

        initValidation(facesContext, uiComponent, metaDataEntry, convertedObject);

        logger.finest("initValidation of " + getClass().getName() + " finished");

        try
        {
            logger.finest("start processValidation of " + getClass().getName());

            processValidation(facesContext, uiComponent, metaDataEntry, convertedObject);

            logger.finest("processValidation of " + getClass().getName() + " finished");
        }
        catch (ValidatorException e)
        {
            logger.finest("start processAfterValidatorException of " + getClass().getName());

            ValidatorException validatorException;

            if(e instanceof RequiredValidatorException)
            {
                validatorException = new RequiredValidatorException(
                        ExtValUtils.convertFacesMessage(e.getFacesMessage()), e.getCause());
            }
            else
            {
                validatorException = new ValidatorException(
                        ExtValUtils.convertFacesMessage(e.getFacesMessage()), e.getCause());
            }
            
            if (processAfterValidatorException(
                    facesContext, uiComponent, metaDataEntry, convertedObject, validatorException))
            {
                logger.finest(getClass().getName() +
                    ": throw original exception after processAfterValidatorException");

                ExtValUtils.tryToThrowValidatorExceptionForComponent(
                        uiComponent, validatorException.getFacesMessage(), validatorException);
            }

            logger.finest(getClass().getName() +
                ": original exception after processAfterValidatorException not thrown");
        }
    }

    /**
     * Allows some initialization before the actual validation is performed. This initialization is executed before
     * each validation and not only once when th class is constructed.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The JSF component that contained the value entered by the user.
     * @param metaDataEntry The data holder which stores the meta-data and some information where the meta-data was
     * around.
     * @param convertedObject Converted object of the user entered value.
     */
    protected void initValidation(FacesContext facesContext,
                                  UIComponent uiComponent,
                                  MetaDataEntry metaDataEntry,
                                  Object convertedObject)
    {
        //override if needed
    }

    //override if needed
    /**
     * Executes the registered ValidationExceptionInterceptor to process the validation exception.
     * 
     * @param facesContext The JSF Context
     * @param uiComponent The JSF component that contained the value entered by the user.
     * @param metaDataEntry The data holder which stores the meta-data and some information where the meta-data was
     * around.
     * @param convertedObject Converted object of the user entered value.
     * @param validatorException The validation exception that occurred.
     * @return should return true when you like to have the ValidatorException thrown by the
     * ExtValUtils#tryToThrowValidatorExceptionForComponent method.
     * {@see org.apache.myfaces.extensions.validator.util.ExtValUtils#tryToThrowValidatorExceptionForComponent(javax.faces.component.UIComponent, javax.faces.application.FacesMessage, java.lang.Throwable)}
     */
    protected boolean processAfterValidatorException(FacesContext facesContext,
                                                     UIComponent uiComponent,
                                                     MetaDataEntry metaDataEntry,
                                                     Object convertedObject,
                                                     ValidatorException validatorException)
    {
        return ExtValUtils.executeAfterThrowingInterceptors(
                uiComponent, metaDataEntry, convertedObject, validatorException, this);
    }

    /**
    /**
     * Validates the value in the convertedObject parameter which the user entered as value of the uiComponent.
     * Additional validation information can be found in the metaDataEntry parameter. The method throws a
     * ValidatorException when violation of the validation rules occurs.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The JSF component that contained the value entered by the user.
     * @param metaDataEntry The data holder which stores the meta-data and some information where the meta-data was
     * around.
     * @param convertedObject Converted object of the user entered value.
     * @throws ValidatorException When violation of the validation rules occurs.
     */
    protected abstract void processValidation(FacesContext facesContext,
                                              UIComponent uiComponent, MetaDataEntry metaDataEntry,
                                              Object convertedObject) throws ValidatorException;
}
