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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.FactoryUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;
import javax.faces.validator.ValidatorException;
import java.util.Map;

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
public abstract class AbstractValidatorAdapter implements ValidationStrategy, ComponentInitializer
{
    protected final Log logger = LogFactory.getLog(getClass());

    public void validate(FacesContext facesContext, UIComponent uiComponent,
                         AnnotationEntry annotationEntry, Object convertedObject)
    {
        initValidation(facesContext, uiComponent, annotationEntry,
            convertedObject);

        try
        {
            processValidation(facesContext, uiComponent, annotationEntry,
                convertedObject);
        }
        catch (ValidatorException e)
        {
            if (processAfterValidatorException(facesContext, uiComponent,
                annotationEntry, convertedObject, e))
            {
                throw new ConverterException(e.getFacesMessage(), e);
            }
        }
    }

    protected void initValidation(FacesContext facesContext,
                                  UIComponent uiComponent, AnnotationEntry annotationEntry,
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

    public final void configureComponent(FacesContext facesContext,
                                         UIComponent uiComponent,
                                         Map<String, Object> metaData)
    {
        getComponentInitializer(uiComponent).configureComponent(facesContext, uiComponent, metaData);
    }

    protected ComponentInitializer getComponentInitializer(UIComponent uiComponent)
    {
        return FactoryUtils.getComponentInitializerFactory().create(uiComponent);
    }

    protected abstract void processValidation(FacesContext facesContext,
                                              UIComponent uiComponent, AnnotationEntry annotationEntry,
                                              Object convertedObject) throws ValidatorException;
}
