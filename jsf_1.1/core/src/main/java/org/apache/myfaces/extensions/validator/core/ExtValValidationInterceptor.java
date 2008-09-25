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
package org.apache.myfaces.extensions.validator.core;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.AnnotationExtractor;
import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.util.FactoryUtils;
import org.apache.myfaces.extensions.validator.util.ValidationUtils;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.component.EditableValueHolder;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;
import java.lang.reflect.Method;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValValidationInterceptor extends AbstractRendererInterceptor
{
    private static Boolean isAlternativeAvailable = null;

    @Override
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
        throws IOException
    {
        initComponent(facesContext, uiComponent);
    }

    protected void initComponent(FacesContext facesContext, UIComponent uiComponent)
    {
        if(!(uiComponent instanceof EditableValueHolder))
        {
            return;
        }

        ValidationStrategy validationStrategy;
        MetaDataExtractor metaDataExtractor;

        AnnotationExtractor annotationExtractor = FactoryUtils.getComponentAnnotationExtractorFactory().create();

        Map<String, Object> metaData;
        for (AnnotationEntry entry : annotationExtractor.extractAnnotations(facesContext, uiComponent))
        {
            validationStrategy = FactoryUtils.getValidationStrategyFactory().create(entry.getAnnotation());

            if (validationStrategy != null)
            {
                metaDataExtractor = FactoryUtils.getMetaDataExtractorFactory().create(validationStrategy);

                if(metaDataExtractor != null)
                {
                    metaData = metaDataExtractor.extractMetaData(entry.getAnnotation());
                }
                else
                {
                    metaData = null;
                }

                if(metaData == null)
                {
                    metaData = new HashMap<String, Object>();
                }

                FactoryUtils.getComponentInitializerFactory().create(uiComponent)
                    .configureComponent(facesContext, uiComponent, metaData);
            }
        }
    }

    @Override
    public void beforeGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o, Renderer wrapped)
        throws ConverterException
    {
        checkForAlternative();

        //if the user activated an alternative mode, cancel here (in order to avoid double validation)
        if (Boolean.TRUE.equals(isAlternativeAvailable))
        {
            return;
        }

        Object convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);
        ValidationUtils.processExtValValidation(facesContext, uiComponent, convertedObject);
    }

    private void checkForAlternative()
    {
        //to avoid a config parameter (but not nice)
        if(isAlternativeAvailable == null)
        {
            Class extValApplicationFactoryClass = ClassUtils.tryToLoadClassForName(
                "org.apache.myfaces.extensions.validator.core.proxy.ExtValApplicationFactory");

            Method isActiveMethod = ReflectionUtils.tryToGetMethod(extValApplicationFactoryClass, "isActive", null);
            isAlternativeAvailable = (Boolean)ReflectionUtils
                .tryToInvokeMethodOfClass(extValApplicationFactoryClass, isActiveMethod);

            if(isAlternativeAvailable == null)
            {
                isAlternativeAvailable = false;
            }
        }
    }
}
