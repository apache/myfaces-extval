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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;

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
    protected final Log logger = LogFactory.getLog(getClass());

    protected AbstractValidationStrategy()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    public void validate(FacesContext facesContext, UIComponent uiComponent,
                         MetaDataEntry metaDataEntry, Object convertedObject)
    {
        if(logger.isTraceEnabled())
        {
            logger.trace("start initValidation of " + getClass().getName());
        }

        initValidation(facesContext, uiComponent, metaDataEntry, convertedObject);

        if(logger.isTraceEnabled())
        {
            logger.trace("initValidation of " + getClass().getName() + " finished");
        }

        try
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("start processValidation of " + getClass().getName());
            }

            processValidation(facesContext, uiComponent, metaDataEntry, convertedObject);

            if(logger.isTraceEnabled())
            {
                logger.trace("processValidation of " + getClass().getName() + " finished");
            }
        }
        catch (ValidatorException e)
        {
            if(logger.isTraceEnabled())
            {
                logger.trace("start processAfterValidatorException of " + getClass().getName());
            }

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
                if(logger.isTraceEnabled())
                {
                    logger.trace(getClass().getName() +
                        ": throw original exception after processAfterValidatorException");
                }

                ExtValUtils.tryToThrowValidatorExceptionForComponent(
                        uiComponent, validatorException.getFacesMessage(), validatorException);
            }

            if(logger.isTraceEnabled())
            {
                logger.trace(getClass().getName() +
                    ": original exception after processAfterValidatorException not thrown");
            }
        }
    }

    protected void initValidation(FacesContext facesContext,
                                  UIComponent uiComponent,
                                  MetaDataEntry metaDataEntry,
                                  Object convertedObject)
    {
        //override if needed
    }

    //override if needed
    protected boolean processAfterValidatorException(FacesContext facesContext,
                                                     UIComponent uiComponent,
                                                     MetaDataEntry metaDataEntry,
                                                     Object convertedObject,
                                                     ValidatorException validatorException)
    {
        return ExtValUtils.executeAfterThrowingInterceptors(
                uiComponent, metaDataEntry, convertedObject, validatorException, this);
    }

    protected abstract void processValidation(FacesContext facesContext,
                                              UIComponent uiComponent, MetaDataEntry metaDataEntry,
                                              Object convertedObject) throws ValidatorException;
}
