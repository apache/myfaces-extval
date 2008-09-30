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

import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.LogUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;
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
public abstract class AbstractValidatorAdapter implements ValidationStrategy
{
    public void validate(FacesContext facesContext, UIComponent uiComponent,
                         AnnotationEntry annotationEntry, Object convertedObject)
    {
        LogUtils.trace("start initValidation of " + getClass().getName(), getClass());

        initValidation(facesContext, uiComponent, annotationEntry, convertedObject);

        LogUtils.trace("initValidation of " + getClass().getName() + " finished", getClass());

        try
        {
            LogUtils.trace("start processValidation of " + getClass().getName(), getClass());

            processValidation(facesContext, uiComponent, annotationEntry, convertedObject);

            LogUtils.trace("processValidation of " + getClass().getName() + " finished", getClass());
        }
        catch (ValidatorException e)
        {
            LogUtils.trace("start processAfterValidatorException of " + getClass().getName(), getClass());

            if (processAfterValidatorException(facesContext, uiComponent,
                annotationEntry, convertedObject, e))
            {
                LogUtils.trace(getClass().getName() +
                    ": throw original exception after processAfterValidatorException", getClass());

                throw new ConverterException(e.getFacesMessage(), e);
            }

            LogUtils.trace(getClass().getName() +
                ": original exception after processAfterValidatorException not thrown", getClass());
        }
    }

    protected void initValidation(FacesContext facesContext,
                                  UIComponent uiComponent,
                                  AnnotationEntry annotationEntry,
                                  Object convertedObject)
    {
        //override if needed
    }

    //override if needed
    protected boolean processAfterValidatorException(FacesContext facesContext,
                                                     UIComponent uiComponent, AnnotationEntry annotationEntry,
                                                     Object convertedObject, ValidatorException e)
    {
        return true;
    }

    protected abstract void processValidation(FacesContext facesContext,
                                              UIComponent uiComponent, AnnotationEntry annotationEntry,
                                              Object convertedObject) throws ValidatorException;
}
