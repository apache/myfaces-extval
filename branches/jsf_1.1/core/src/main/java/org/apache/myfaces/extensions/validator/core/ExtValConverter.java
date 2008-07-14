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

import net.sf.cglib.proxy.Enhancer;
import net.sf.cglib.proxy.MethodInterceptor;
import net.sf.cglib.proxy.MethodProxy;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.AnnotationExtractor;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.util.ELUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.FactoryUtils;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author Gerhard Petracek
 */
public class ExtValConverter implements Converter, MethodInterceptor, Serializable {
    protected final Log logger = LogFactory.getLog(getClass());

    public static Converter newInstance(Converter wrappedConverter) {
        Enhancer enhancer = new Enhancer();
        enhancer.setSuperclass(wrappedConverter.getClass());
        enhancer.setInterfaces(new Class[]{Converter.class, Serializable.class});
        enhancer.setCallback(new ExtValConverter());

        ExtValUtils.increaseProcessedConverterCount();

        return (Converter) enhancer.create();
    }

    public ExtValConverter() {
        logger.trace("myfaces-extension-validator converter instantiated");
    }

    public Object getAsObject(FacesContext facesContext, UIComponent uiComponent, String s) {

        Object convertedObject = getConvertedObject(facesContext, uiComponent, s);

        processExtValValidation(facesContext, uiComponent, convertedObject);

        return convertedObject;
    }

    public String getAsString(FacesContext facesContext, UIComponent uiComponent, Object o) {
        //indirect approach for complex components
        Converter converter = ExtValUtils.tryToCreateOriginalConverter(facesContext, uiComponent);
        return (converter == null) ? (o == null) ? null : o.toString() : converter.getAsString(facesContext, uiComponent, o);
    }

    public Object intercept(Object obj, Method method, Object[] args, MethodProxy proxy) throws Throwable {
        Object convertedObject = proxy.invokeSuper(obj, args);

        if (method.getName().equals("getAsObject")) {
            processExtValValidation((FacesContext) args[0], (UIComponent) args[1], convertedObject);
        } else if (method.getName().equals("getAsString")) {
            storeComponentConverterMappingForProxies((FacesContext) args[0], (UIComponent) args[1], (Converter)obj);
        }
        return convertedObject;
    }

    protected Object getConvertedObject(FacesContext facesContext, UIComponent uiComponent, String s) {
        //indirect approach for complex components
        //TODO
        Converter converter = ExtValUtils.tryToCreateOriginalConverter(facesContext, uiComponent);
        return (converter != null) ? converter.getAsObject(facesContext, uiComponent, s) : s;
    }

    /*
     * private methods
     */
    private void createValueBindingConvertedValueMapping(UIComponent uiComponent, Object convertedObject) {
        //to support local cross-validation (within the same entity)
        Map<String, ProcessedInformationEntry> valueBindingConvertedValueMapping = ExtValUtils.getOrInitValueBindingConvertedValueMapping();

        String valueBindingExpression;
        ProcessedInformationEntry entry;

        valueBindingExpression = ELUtils.getReliableValueBindingExpression(uiComponent);

        if (valueBindingExpression == null) {
            return;
        }

        entry = new ProcessedInformationEntry();
        entry.setBean(ELUtils.getBeanObject(valueBindingExpression, uiComponent));
        entry.setConvertedValue(convertedObject);
        entry.setComponent(uiComponent);

        //for local cross-validation
        if (valueBindingConvertedValueMapping.containsKey(valueBindingExpression) && !valueBindingConvertedValueMapping.get(valueBindingExpression).getBean().equals(entry.getBean())) {
            //for the validation within a complex component e.g. a table
            //don't override existing expression (style: #{entry.property}) - make a special mapping

            List<ProcessedInformationEntry> furtherEntries = valueBindingConvertedValueMapping.get(valueBindingExpression).getFurtherEntries();
            if (furtherEntries == null) {
                furtherEntries = new ArrayList<ProcessedInformationEntry>();

                valueBindingConvertedValueMapping.get(valueBindingExpression).setFurtherEntries(furtherEntries);
            }

            furtherEntries.add(entry);
        } else {
            //for normal validation
            valueBindingConvertedValueMapping.put(valueBindingExpression, entry);
        }
    }

    private void processExtValValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject) {
        if (uiComponent instanceof EditableValueHolder) {
            ValidationStrategy validationStrategy;

            AnnotationExtractor annotationExtractor = FactoryUtils.getAnnotationExtractorFactory().create();
            for (AnnotationEntry entry : annotationExtractor.extractAnnotations(facesContext, uiComponent)) {
                validationStrategy = FactoryUtils.getValidationStrategyFactory().create(entry.getAnnotation());

                if (validationStrategy != null) {
                    validationStrategy.validate(facesContext, uiComponent, entry, convertedObject);
                } else {
                    logger.trace("no validation strategy found for " + entry.getAnnotation().annotationType().getName());
                }
            }

            //build mapping value-binding -> processed information entry
            createValueBindingConvertedValueMapping(uiComponent, convertedObject);
        }
    }


    private void storeComponentConverterMappingForProxies(FacesContext facesContext, UIComponent uiComponent, Converter converter) {
        if (ExtValUtils.useProxyMapping()) {
            ExtValUtils.getOrInitProxyMapping().put(uiComponent.getClientId(facesContext), converter);
            ExtValUtils.decreaseProcessedConverterCount();
        }
    }
}
