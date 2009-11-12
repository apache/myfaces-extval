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

import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.AnnotationExtractor;
import org.apache.myfaces.extensions.validator.core.client.HTML;
import org.apache.myfaces.extensions.validator.core.proxy.ExtValApplicationFactory;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ClientSideValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.internal.UsageEnum;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.FactoryUtils;
import org.apache.myfaces.extensions.validator.util.ValidationUtils;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import java.io.IOException;

/**
 * Default approach to avoid proxies for converters and the adapter fallback.
 * It requires that components delegate getConvertedValue to a renderer.
 * If it isn't the case for your component lib use:
 * org.apache.myfaces.extensions.validator.core.proxy.ExtValApplicationFactory
 * and
 * org.apache.myfaces.extensions.validator.core.proxy.ProxyMappingPhaseListener
 * <p/>
 * This wrapper will also implement client-side validation behaviour
 *
 * @author Gerhard Petracek
 */
@UsageInformation(UsageEnum.INTERNAL)
public class ExtValRendererWrapper extends Renderer
{
    private static final String HIDDEN_SUBMIT_INPUT_SUFFIX = "_EXTVAL";

    private Renderer wrapped;

    public ExtValRendererWrapper(Renderer wrapped)
    {
        this.wrapped = wrapped;
    }

    public void decode(FacesContext facesContext, UIComponent uiComponent)
    {
        wrapped.decode(facesContext, uiComponent);
    }

    public void encodeBegin(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        wrapped.encodeBegin(facesContext, uiComponent);
    }

    public void encodeChildren(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        wrapped.encodeChildren(facesContext, uiComponent);
    }

    public void encodeEnd(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        wrapped.encodeEnd(facesContext, uiComponent);

        if (!(uiComponent instanceof EditableValueHolder))
        {
            return;
        }

        ValidationStrategy validationStrategy;
        boolean firstEntry = true;

        AnnotationExtractor annotationExtractor = FactoryUtils
            .getAnnotationExtractorFactory().create();

        StringBuffer metaData = new StringBuffer();
        metaData.append("[");

        for (AnnotationEntry entry : annotationExtractor
            .extractAnnotations(facesContext, uiComponent))
        {
            validationStrategy = FactoryUtils
                .getValidationStrategyFactory().create(entry.getAnnotation());

            if (validationStrategy instanceof ClientSideValidationStrategy)
            {
                for (String currentMetaData : ((ClientSideValidationStrategy) validationStrategy)
                    .extractMetaData(entry.getAnnotation()))
                {
                    metaData.append(currentMetaData);

                    if (!firstEntry)
                    {
                        metaData.append(",");
                    }
                    else
                    {
                        firstEntry = false;
                    }
                }
            }
        }
        metaData.append("]");

        ResponseWriter responseWriter = facesContext.getResponseWriter();

        responseWriter.startElement(HTML.INPUT_ELEM, uiComponent);
        responseWriter.writeAttribute(HTML.TYPE_ATTR, HTML.INPUT_TYPE_HIDDEN, null);
        responseWriter.writeAttribute(HTML.ID_ATTR, uiComponent.getClientId(facesContext)
            + HIDDEN_SUBMIT_INPUT_SUFFIX, null);
        responseWriter.writeAttribute(HTML.VALUE_ATTR, metaData, null);
        responseWriter.endElement(HTML.INPUT_ELEM);
    }

    public String convertClientId(FacesContext facesContext, String s)
    {
        return wrapped.convertClientId(facesContext, s);
    }

    public boolean getRendersChildren()
    {
        return wrapped.getRendersChildren();
    }

    public Object getConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o)
        throws ConverterException
    {
        Object convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);

        //if the user activated the proxy mode cancel here
        if (ExtValApplicationFactory.isActive())
        {
            return convertedObject;
        }

        ValidationUtils.processExtValValidation(facesContext, uiComponent, convertedObject);

        return convertedObject;
    }
}