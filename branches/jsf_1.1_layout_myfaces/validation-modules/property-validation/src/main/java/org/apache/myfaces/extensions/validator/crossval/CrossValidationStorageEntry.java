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
package org.apache.myfaces.extensions.validator.crossval;

import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.crossval.strategy.CrossValidationStrategy;

import javax.faces.component.UIComponent;

/**
 * @author Gerhard Petracek
 */
public class CrossValidationStorageEntry {
    private AnnotationEntry annotationEntry;
    //for complex components (e.g. a table) stores the object of entry (#{entry.property})
    private Object bean;
    private UIComponent component;
    private Object convertedObject;
    private CrossValidationStrategy validationStrategy;

    public AnnotationEntry getAnnotationEntry() {
        return annotationEntry;
    }

    public void setAnnotationEntry(AnnotationEntry annotationEntry) {
        this.annotationEntry = annotationEntry;
    }

    public Object getBean() {
        return bean;
    }

    public void setBean(Object bean) {
        this.bean = bean;
    }

    public UIComponent getComponent() {
        return component;
    }

    public void setComponent(UIComponent component) {
        this.component = component;
    }

    public Object getConvertedObject() {
        return convertedObject;
    }

    public void setConvertedObject(Object convertedObject) {
        this.convertedObject = convertedObject;
    }

    public CrossValidationStrategy getValidationStrategy() {
        return validationStrategy;
    }

    public void setValidationStrategy(CrossValidationStrategy validationStrategy) {
        this.validationStrategy = validationStrategy;
    }
}
